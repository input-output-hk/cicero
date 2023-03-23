package config

import (
	"database/sql"
	"encoding/json"
	"os"
	"strings"
	"time"

	"github.com/antonlindstrom/pgstore"
	_ "github.com/jackc/pgx/v5/stdlib"
	"github.com/pkg/errors"
	"github.com/zitadel/oidc/v2/pkg/client/rp"
	"github.com/zitadel/oidc/v2/pkg/client/rs"
	oidchttp "github.com/zitadel/oidc/v2/pkg/http"
	"github.com/zitadel/oidc/v2/pkg/oidc"
)

type WebConfig struct {
	Listen        string
	OidcProviders map[string]OidcProvider
	Sessions      *pgstore.PGStore
}

type OidcProvider struct {
	rp.RelyingParty
	rs.ResourceServer
}

func NewWebConfig(listen, cookieAuth, cookieEnc, oidcProviders string) (WebConfig, error) {
	self := WebConfig{Listen: listen}

	var cookieAuthKey, cookieEncKey []byte

	if v, err := os.ReadFile(cookieAuth); err != nil {
		return self, errors.WithMessage(err, "While reading web cookie authentication key")
	} else {
		cookieAuthKey = v
	}
	if v, err := os.ReadFile(cookieEnc); err != nil {
		return self, errors.WithMessage(err, "While reading web cookie encryption key")
	} else {
		cookieEncKey = v
	}

	oidcProviderCfgs := make(map[string]struct {
		Issuer       string `json:"issuer"`
		ClientId     string `json:"client-id"`
		ClientSecret string `json:"client-secret"`
		CallbackUrl  string `json:"callback-url"`
	})
	if providersJson, err := os.ReadFile(oidcProviders); err != nil {
		return self, errors.WithMessage(err, "While reading web OIDC provider settings")
	} else if err := json.Unmarshal(providersJson, &oidcProviderCfgs); err != nil {
		return self, errors.WithMessage(err, "While unmarshaling web OIDC provider settings")
	}
	self.OidcProviders = make(map[string]OidcProvider, len(oidcProviderCfgs))
	for name, cfg := range oidcProviderCfgs {
		if provider, err := rp.NewRelyingPartyOIDC(
			cfg.Issuer,
			cfg.ClientId,
			cfg.ClientSecret,
			cfg.CallbackUrl,
			[]string{oidc.ScopeOpenID, oidc.ScopeEmail},
			// OAuth 2.1 and OAuth 2.0 BCP mandate PKCE for code flow regardless of whether a client secret is used
			rp.WithPKCE(oidchttp.NewCookieHandler(cookieAuthKey, cookieEncKey)),
			rp.WithVerifierOpts(rp.WithIssuedAtOffset(5*time.Second)),
		); err != nil {
			return self, errors.WithMessagef(err, "While setting up OIDC provider: %q", name)
		} else {
			resourceServer, err := rs.NewResourceServerClientCredentials(cfg.Issuer, cfg.ClientId, cfg.ClientSecret)
			if err != nil {
				if strings.HasPrefix(err.Error(), "introspectURL and/or tokenURL is empty:") {
					resourceServer = nil
				} else {
					return self, errors.WithMessagef(err, "While setting up OIDC resource server: %q", name)
				}
			}
			self.OidcProviders[name] = OidcProvider{provider, resourceServer}
		}
	}

	if url, err := DbUrl(); err != nil {
		return self, err
	} else if db, err := sql.Open("pgx", url); err != nil {
		return self, err
	} else if store, err := pgstore.NewPGStoreFromPool(db, cookieAuthKey, cookieEncKey); err != nil {
		return self, err
	} else {
		self.Sessions = store
	}

	return self, nil
}
