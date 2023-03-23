package service

import (
	"context"
	"io"
	"testing"

	"github.com/pashagolub/pgxmock/v2"
	"github.com/rs/zerolog"
	"github.com/stretchr/testify/assert"

	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/util"
)

func TestMatch(t *testing.T) {
	t.Parallel()

	// given
	db, err := pgxmock.NewConn()
	if err != nil {
		t.Fatalf("an error was not expected when opening a stub database connection: %q", err)
	}
	defer db.Close(context.Background())
	logger := zerolog.New(io.Discard)
	factService := NewFactService(db, nil, &logger)

	tries := map[string]struct {
		factValue map[string]any
		matchCue  string
		callback  func(*testing.T, error)
	}{
		"exact": {
			map[string]any{
				"ok":       true,
				"revision": "sha",
			},
			`ok: bool, revision: string`,
			func(t *testing.T, err error) {
				assert.Nil(t, err)
			},
		},
		"missing field": {
			map[string]any{
				"revision": "sha",
			},
			`ok: bool, revision: string`,
			func(t *testing.T, err error) {
				assert.Error(t, err)
			},
		},
		"missing field and extra field": {
			map[string]any{
				"revision": "sha",
				"extra":    false,
			},
			`ok: bool, revision: string`,
			func(t *testing.T, err error) {
				assert.Error(t, err)
			},
		},
		"extra field": {
			map[string]any{
				"ok":       true,
				"revision": "sha",
				"extra":    false,
			},
			`ok: bool, revision: string`,
			func(t *testing.T, err error) {
				assert.Nil(t, err)
			},
		},
		"forbidden field": {
			map[string]any{
				"ok": true,
				"commit": map[string]any{
					"revision": "sha",
					"extra":    false,
				},
			},
			`ok: bool, commit: close({revision: string})`,
			func(t *testing.T, err error) {
				assert.Error(t, err)
			},
		},
		"string interpolation with reference": {
			map[string]any{
				"ref": "refs/heads/master",
				"repo": map[string]any{
					"default_branch": "master",
				},
			},
			`ref: "refs/heads/\(repo.default_branch)", repo: default_branch: string`,
			func(t *testing.T, err error) {
				assert.Nil(t, err)
			},
		},
		"unrelated concrete": {
			map[string]any{
				"foo": map[string]any{
					"bar": "bar",
				},
			},
			`qux: true`,
			func(t *testing.T, err error) {
				assert.Error(t, err)
			},
		},
	}

	for k, try := range tries {
		// copy to avoid pointing to loop variables
		k := k
		try := try

		fact := domain.Fact{
			Value: try.factValue,
		}
		match := util.CUEString(try.matchCue).Value(nil, nil)

		t.Run(k, func(t *testing.T) {
			t.Parallel()

			// when
			matchErr, err := factService.Match(&fact, match)

			// then
			assert.Nil(t, err)
			try.callback(t, matchErr)

			if t.Failed() {
				unified := match.Context().Encode(fact.Value).Unify(match)

				t.Logf("fact: %v", try.factValue)
				t.Logf("cue: %s", try.matchCue)
				t.Logf("unified: %s", unified)
				if err := unified.Err(); err != nil {
					t.Logf("unified error: %s", err.Error())
				}
				if matchErr != nil {
					t.Logf("match error: %s", matchErr.Error())
				}
			}
		})

	}
}
