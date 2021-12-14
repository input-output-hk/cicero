package config

import (
	"fmt"
	"github.com/rs/zerolog"
	"io/ioutil"
	"os"
	"path/filepath"
	"testing"

	"github.com/liftbridge-io/liftbridge/server"
	"github.com/stretchr/testify/require"
)

var storagePath string

func init() {
	tmpDir, err := ioutil.TempDir("", "liftbridge_test_")
	if err != nil {
		panic(fmt.Errorf("Error creating temp dir: %v", err))
	}
	if err := os.Remove(tmpDir); err != nil {
		panic(fmt.Errorf("Error removing temp dir: %v", err))
	}
	storagePath = tmpDir
}

func cleanupStorage(t *testing.T) {
	err := os.RemoveAll(storagePath)
	require.NoError(t, err)
}

func getTestConfig(id string, bootstrap bool, port int) *server.Config {
	config := server.NewDefaultConfig()
	config.Clustering.RaftBootstrapSeed = bootstrap
	config.DataDir = filepath.Join(storagePath, id)
	config.Clustering.RaftSnapshots = 1
	config.LogRaft = true
	config.Clustering.ServerID = id
	config.LogLevel = uint32(zerolog.DebugLevel)
	config.EmbeddedNATS = bootstrap
	config.LogSilent = true
	config.Port = port
	return config
}

func runServerWithConfig(t *testing.T, config *server.Config) *server.Server {
	s, err := server.RunServerWithConfig(config)
	require.NoError(t, err)
	return s
}

func startLiftbridge(t *testing.T) *server.Server {
	// Configure the first server as a seed.
	s1Config := getTestConfig("a", true, 0)
	s1 := runServerWithConfig(t, s1Config)
	defer func() { _ = s1.Stop() }()

	// Configure second server which should automatically join the first.
	s2Config := getTestConfig("b", false, 0)
	s2 := runServerWithConfig(t, s2Config)
	defer func() { _ = s2.Stop() }()

	var leader *server.Server
	for {
		if s1.IsLeader() {
			leader = s1
			break
		}

		if s2.IsLeader() {
			leader = s2
			break
		}
	}

	return leader
}

func TestBrain(t *testing.T) {
	defer cleanupStorage(t)
	startLiftbridge(t)
}
