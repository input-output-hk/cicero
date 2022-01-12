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
		var i int
		return &i, nil
	}

	v, err := strconv.Atoi(s)
	if err != nil {
		var i int
		return &i, err
	}
	return &v, nil
}

func GetenvBool(key string) (*bool, error) {
	s := GetenvStr(key)
	if s == "" {
		b := false
		return &b, nil
	}

	v, err := strconv.ParseBool(s)
	if err != nil {
		b := false
		return &b, err
	}
	return &v, nil
}
