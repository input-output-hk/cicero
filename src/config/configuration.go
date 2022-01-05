package config

import (
	"github.com/pkg/errors"
	"os"
	"strconv"
)

func GetenvStr(key string) (string, error) {
	v := os.Getenv(key)
	if v == "" {
		return v, errors.Errorf("Environment Variable %s is not set or empty", key)
	}
	return v, nil
}

func GetenvInt(key string) (int, error) {
	s, err := GetenvStr(key)
	if err != nil {
		return 0, err
	}
	v, err := strconv.Atoi(s)
	if err != nil {
		return 0, err
	}
	return v, nil
}

func GetenvBool(key string) (bool, error) {
	s, err := GetenvStr(key)
	if err != nil {
		return false, err
	}
	v, err := strconv.ParseBool(s)
	if err != nil {
		return false, err
	}
	return v, nil
}
