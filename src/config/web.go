package config

import (
	"encoding/json"
	"os"
	"time"

	"github.com/pkg/errors"
	"github.com/zitadel/oidc/pkg/client/rp"
	oidchttp "github.com/zitadel/oidc/pkg/http"
	"github.com/zitadel/oidc/pkg/oidc"
)

type WebConfig struct {
	Listen        string
	CookieAuth    []byte
	CookieEnc     []byte
	OidcProviders map[string]rp.RelyingParty
}

func NewWebConfig(listen, cookieAuth, cookieEnc, oidcProviders string) (WebConfig, error) {
	self := WebConfig{Listen: listen}

	if v, err := os.ReadFile(cookieAuth); err != nil {
		return self, errors.WithMessage(err, "While reading web cookie authentication key")
	} else {
		self.CookieAuth = v
	}
	if v, err := os.ReadFile(cookieEnc); err != nil {
		return self, errors.WithMessage(err, "While reading web cookie encryption key")
	} else {
		self.CookieEnc = v
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
	self.OidcProviders = make(map[string]rp.RelyingParty, len(oidcProviderCfgs))
	for name, cfg := range oidcProviderCfgs {
		if provider, err := rp.NewRelyingPartyOIDC(
			cfg.Issuer,
			cfg.ClientId,
			cfg.ClientSecret,
			cfg.CallbackUrl,
			[]string{oidc.ScopeOpenID, oidc.ScopeEmail},
			// OAuth 2.1 and OAuth 2.0 BCP mandate PKCE for code flow regardless of whether a client secret is used
			rp.WithPKCE(oidchttp.NewCookieHandler(self.CookieAuth, self.CookieEnc)),
			rp.WithVerifierOpts(rp.WithIssuedAtOffset(5*time.Second)),
		); err != nil {
			return self, errors.WithMessagef(err, "While setting up OIDC provider: %q", name)
		} else {
			self.OidcProviders[name] = provider
		}
	}

	return self, nil
}