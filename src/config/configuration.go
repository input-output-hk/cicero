package config

import (
	"os"
	"strconv"
)

func GetenvStr(key string) string {
	return os.Getenv(key)
}

func GetenvInt(key string) (*int, error) {
	s := GetenvStr(key)
	if s == "" {
		return nil, nil
	}

	v, err := strconv.Atoi(s)
	if err != nil {
		return nil, err
	}
	return &v, nil
}

func GetenvBool(key string) (result *bool, err error) {
	s := GetenvStr(key)
	if s == "" {
		return
	}

	v, err := strconv.ParseBool(s)
	if err != nil {
		return nil, err
	}
	return &v, nil
}
