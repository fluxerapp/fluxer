// SPDX-License-Identifier: AGPL-3.0-or-later

import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {
	canOpenBrowserVoiceDebugEventSinkPopout,
	setBrowserVoiceDebugEventSinkStatsHtml,
} from '@app/features/voice/diagnostics/VoiceDebugBrowserEventSinkPopout';
import {
	renderVoiceDebugStatsHtml,
	renderVoiceDebugStatsUnavailableHtml,
} from '@app/features/voice/diagnostics/VoiceDebugStatsHtml';
import voiceEngineV2AppDebugEventSinkHostAdapter from '@app/features/voice/engine/v2/VoiceEngineV2AppDebugEventSinkHostAdapter';
import {collectStatsForNerdsSnapshot} from '@app/features/voice/utils/StatsForNerdsCopy';

export function canOpenVoiceDebugEventSinkPopout(): boolean {
	return Boolean(getElectronAPI()?.openVoiceDebugEventSinkPopout) || canOpenBrowserVoiceDebugEventSinkPopout();
}

function getGeneratedAtIso(): string {
	return new Date().toISOString();
}

function describeStatsSnapshotError(error: unknown): string {
	return error instanceof Error ? error.message : String(error);
}

function publishBrowserOpeningStatsSnapshot(): void {
	if (!canOpenBrowserVoiceDebugEventSinkPopout()) return;
	const generatedAtIso = getGeneratedAtIso();
	try {
		setBrowserVoiceDebugEventSinkStatsHtml(renderVoiceDebugStatsHtml(collectStatsForNerdsSnapshot(), generatedAtIso));
	} catch (error) {
		setBrowserVoiceDebugEventSinkStatsHtml(
			renderVoiceDebugStatsUnavailableHtml(
				`Failed to collect stats snapshot before opening event sink: ${describeStatsSnapshotError(error)}`,
				generatedAtIso,
			),
		);
	}
}

export async function openVoiceDebugEventSinkPopout(): Promise<void> {
	publishBrowserOpeningStatsSnapshot();
	await voiceEngineV2AppDebugEventSinkHostAdapter.openEventSinkPopout();
}
