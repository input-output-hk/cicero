package config

import (
	"io"
	"os"
	"path"
	"time"

	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
	"gopkg.in/natefinch/lumberjack.v2"
)

type SupervisorLogger struct {
	*zerolog.Logger
}

func (l *SupervisorLogger) Printf(format string, v ...interface{}) {
	l.Logger.Printf(format, v...)
}
func (l *SupervisorLogger) Println(v ...interface{}) {
	l.Logger.Print(v...)
}

type LoggerConfig struct {
	// Print human-readable output to console
	ConsoleLoggingEnabled bool

	// FileLoggingEnabled makes the framework log to a file
	// the fields below can be skipped if this value is false!
	FileLoggingEnabled bool
	// Directory to log to to when filelogging is enabled
	Directory string
	// Filename is the name of the logfile which will be placed inside the directory
	Filename string
	// MaxSize the max size in MB of the logfile before it's rolled
	MaxSize int
	// MaxBackups the max number of rolled files to keep
	MaxBackups int
	// MaxAge the max age in days to keep a logfile
	MaxAge int
}

func buildLoggerConfig() (*LoggerConfig, error) {
	var conf LoggerConfig

	if v, err := GetenvBool("CONSOLE_LOGGING_ENABLED"); err != nil {
		return nil, err
	} else if v != nil {
		conf.ConsoleLoggingEnabled = *v
	}

	if v, err := GetenvBool("FILE_LOGGING_ENABLED"); err != nil {
		return nil, err
	} else if v != nil && *v {
		if v := GetenvStr("LOGS_DIRECTORY"); v == "" {
			conf.Directory = "logs"
		} else {
			conf.Directory = v
		}

		if v := GetenvStr("LOGS_FILE_NAME"); v == "" {
			conf.Filename = "cicero.log"
		} else {
			conf.Filename = v
		}

		switch v, err := GetenvInt("LOGS_MAX_SIZE"); {
		case err != nil:
			return nil, err
		case v != nil:
			conf.MaxSize = *v
		default:
			conf.MaxSize = 10
		}

		switch v, err := GetenvInt("LOGS_MAX_BACKUPS"); {
		case err != nil:
			return nil, err
		case v != nil:
			conf.MaxBackups = *v
		default:
			conf.MaxBackups = 10
		}

		switch v, err := GetenvInt("LOGS_MAX_AGE"); {
		case err != nil:
			return nil, err
		case v != nil:
			conf.MaxAge = *v
		default:
			conf.MaxAge = 10
		}
	}

	return &conf, nil
}

func ConfigureLogger(level zerolog.Level) *zerolog.Logger {
	config, err := buildLoggerConfig()
	if err != nil {
		log.Fatal().Err(err).Msg("can't get logger config")
		return nil
	}

	var writers []io.Writer
	if config.ConsoleLoggingEnabled {
		writers = append(writers, zerolog.NewConsoleWriter(func(w *zerolog.ConsoleWriter) {
			w.Out = os.Stderr
			w.TimeFormat = time.RFC3339
		}))
	} else {
		writers = append(writers, os.Stderr)
	}
	if config.FileLoggingEnabled {
		writers = append(writers, newRollingFile(config))
	}

	logger := zerolog.New(zerolog.SyncWriter(zerolog.MultiLevelWriter(writers...))).
		With().Timestamp().Logger()

	zerolog.SetGlobalLevel(level)

	logger.Info().
		Stringer("logLevel", zerolog.GlobalLevel()).
		Bool("consoleLogging", config.ConsoleLoggingEnabled).
		Bool("fileLogging", config.FileLoggingEnabled).
		Str("logDirectory", config.Directory).
		Str("fileName", config.Filename).
		Int("maxSizeMB", config.MaxSize).
		Int("maxBackups", config.MaxBackups).
		Int("maxAgeInDays", config.MaxAge).
		Msg("logging configured")

	return &logger
}

func newRollingFile(config *LoggerConfig) io.Writer {
	if err := os.MkdirAll(config.Directory, 0o744); err != nil {
		log.Fatal().Err(err).Str("path", config.Directory).Msg("can't create log directory")
		return nil
	}

	return &lumberjack.Logger{
		Filename:   path.Join(config.Directory, config.Filename),
		MaxBackups: config.MaxBackups, // files
		MaxSize:    config.MaxSize,    // megabytes
		MaxAge:     config.MaxAge,     // days
	}
}
