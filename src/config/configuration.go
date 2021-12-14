package config

import (
	"github.com/pkg/errors"
	"github.com/rs/zerolog/log"
	"os"
	"strconv"
)

func getenvStr(key string) (string, error) {
	v := os.Getenv(key)
	if v == "" {
		return v, errors.Errorf("Environment Variable %s Empty", key)
	}
	log.Info().Msgf(">>>>>>>> key %s value: %s", key, v)
	return v, nil
}

func getenvInt(key string) (int, error) {
	s, err := getenvStr(key)
	if err != nil {
		return 0, err
	}
	v, err := strconv.Atoi(s)
	if err != nil {
		return 0, err
	}
	return v, nil
}

func getenvBool(key string) (bool, error) {
	s, err := getenvStr(key)
	if err != nil {
		return false, err
	}
	v, err := strconv.ParseBool(s)
	if err != nil {
		return false, err
	}
	return v, nil
}
