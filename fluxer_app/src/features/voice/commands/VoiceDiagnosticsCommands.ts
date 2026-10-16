// SPDX-License-Identifier: AGPL-3.0-or-later

import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {COPIED_STATS_JSON_DESCRIPTOR} from '@app/features/voice/components/StatsForNerdsCopyDescriptors';
import {buildStatsForNerdsCopyPayload, collectStatsForNerdsSnapshot} from '@app/features/voice/utils/StatsForNerdsCopy';
import type {I18n} from '@lingui/core';

export async function copyVoiceDiagnostics(i18n: I18n): Promise<void> {
	const data = collectStatsForNerdsSnapshot();
	let payload: Record<string, unknown>;
	try {
		payload = await buildStatsForNerdsCopyPayload(data);
	} catch {
		payload = {
			schemaVersion: 1,
			createdAt: new Date().toISOString(),
			statsForNerds: data,
		};
	}
	await navigator.clipboard.writeText(JSON.stringify(payload, null, 2));
	ToastCommands.createToast({type: 'success', children: i18n._(COPIED_STATS_JSON_DESCRIPTOR)});
}
