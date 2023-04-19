package persistence

import (
	"context"

	"github.com/georgysavva/scany/v2/pgxscan"
	"github.com/google/uuid"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/input-output-hk/cicero/src/util"
)

type statisticsRepository struct {
	Db config.PgxIface
}

func NewStatisticsRepository(db config.PgxIface) repository.StatisticsRepository {
	return &statisticsRepository{db}
}

func (self statisticsRepository) WithQuerier(querier config.PgxIface) repository.StatisticsRepository {
	return &statisticsRepository{querier}
}

func (self statisticsRepository) ActionStatus(actionIds []uuid.UUID, actionNames []string, private util.MayBool) (repository.ActionsStatusStatistics, error) {
	result := repository.ActionsStatusStatistics{}
	if err := pgxscan.Select(
		context.Background(), self.Db, &result,
		`SELECT
			action.id                                                  AS id,
			action.name                                                AS name,
			COUNT(invocation.id)                                       AS invocations,
			COUNT(invocation.finished_at)                              AS invocations_finished,
			COUNT(NULLIF(run.status = 'succeeded'::run_status, FALSE)) AS runs_succeeded,
			COUNT(NULLIF(run.status = 'failed'   ::run_status, FALSE)) AS runs_failed,
			COUNT(NULLIF(run.status = 'canceled' ::run_status, FALSE)) AS runs_canceled,
			COUNT(NULLIF(run.status = 'running'  ::run_status, FALSE)) AS runs_running
		FROM action
		JOIN action_name ON action_name.name = action.name
		LEFT JOIN invocation ON invocation.action_id = action.id
		LEFT JOIN run ON run.invocation_id = invocation.id
		WHERE
			(
				$1::bool IS NULL               OR
				action_name.private = $1::bool
			) AND
			(
				action.id   = ANY($2)       OR
				action.name = ANY($3)       OR
				($2 IS NULL AND $3 IS NULL)
			)
		GROUP BY action.id`,
		private.Ptr(), actionIds, actionNames,
	); err != nil {
		return result, err
	}
	return result, nil
}
