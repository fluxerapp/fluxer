// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {useStatsForNerds} from '@app/features/voice/components/useStatsForNerds';
import {
	canOpenBrowserVoiceDebugEventSinkPopout,
	setBrowserVoiceDebugEventSinkStatsHtml,
} from '@app/features/voice/diagnostics/VoiceDebugBrowserEventSinkPopout';
import {
	renderVoiceDebugStatsHtml,
	renderVoiceDebugStatsUnavailableHtml,
} from '@app/features/voice/diagnostics/VoiceDebugStatsHtml';
import type {StatsForNerdsData} from '@app/features/voice/utils/VoiceStatsForNerdsPresenter';
import {useEffect, useRef} from 'react';

const VOICE_DEBUG_STATS_FORWARD_INTERVAL_MS = 2000;
const logger = new Logger('VoiceDebugStatsForwarder');

let activeForwarderCount = 0;

function getGeneratedAtIso(): string {
	return new Date().toISOString();
}

function publishHtml(render: Promise<string>): void {
	void render
		.then((html) => {
			getElectronAPI()?.setVoiceDebugEventSinkStatsHtml?.(html);
			setBrowserVoiceDebugEventSinkStatsHtml(html);
		})
		.catch((error) => {
			logger.warn('Failed to render voice debug stats', error);
		});
}

function publishStatsHtml(data: StatsForNerdsData): void {
	publishHtml(renderVoiceDebugStatsHtml(data, getGeneratedAtIso()));
}

function publishUnavailableStatsHtml(): void {
	publishHtml(
		renderVoiceDebugStatsUnavailableHtml('No active voice call stats snapshot is available.', getGeneratedAtIso()),
	);
}

export function VoiceDebugStatsForwarder(): null {
	const electron = getElectronAPI();
	const enabled = Boolean(electron?.setVoiceDebugEventSinkStatsHtml) || canOpenBrowserVoiceDebugEventSinkPopout();
	const data = useStatsForNerds({enabled});
	const dataRef = useRef(data);
	dataRef.current = data;
	useEffect(() => {
		if (!enabled) return;
		activeForwarderCount += 1;
		publishStatsHtml(dataRef.current);
		const intervalId = window.setInterval(() => {
			publishStatsHtml(dataRef.current);
		}, VOICE_DEBUG_STATS_FORWARD_INTERVAL_MS);
		return () => {
			window.clearInterval(intervalId);
			activeForwarderCount -= 1;
			if (activeForwarderCount === 0) {
				publishUnavailableStatsHtml();
			}
		};
	}, [enabled]);
	return null;
}
