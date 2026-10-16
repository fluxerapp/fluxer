// SPDX-License-Identifier: AGPL-3.0-or-later

import {BatchBuilder} from '@app/api/database/CassandraQueryExecution';
import type {VoiceP2pConnectionReportRow} from '@app/api/database/types/VoiceTypes';
import {VoiceP2pConnectionReports} from '@app/api/Tables';

export class VoiceP2pConnectionReportRepository {
	async insertReports(rows: ReadonlyArray<VoiceP2pConnectionReportRow>): Promise<void> {
		const batch = new BatchBuilder();
		for (const row of rows) {
			batch.addPrepared(VoiceP2pConnectionReports.insert(row));
		}
		await batch.execute();
	}
}
