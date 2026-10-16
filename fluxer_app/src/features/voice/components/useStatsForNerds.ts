// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {getCachedDesktopTroubleshootingSettings} from '@app/features/devtools/utils/DesktopTroubleshootingUtils';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import {collectStatsForNerdsSnapshot, getSystemInfo} from '@app/features/voice/utils/StatsForNerdsCopy';
import type {StatsForNerdsData} from '@app/features/voice/utils/VoiceStatsForNerdsPresenter';
import type {AppMetricsSnapshot, DesktopInfo, GpuInfo} from '@app/types/electron.d';
import {useEffect, useRef, useState} from 'react';

export type {StatsForNerdsData} from '@app/features/voice/utils/VoiceStatsForNerdsPresenter';

const STATS_FOR_NERDS_POLL_INTERVAL_MS = 2000;
const MAX_STATS_HISTORY_SAMPLES = 60;

export interface UseStatsForNerdsOptions {
	enabled?: boolean;
}

class BoundedNumberHistory {
	private readonly samples: Array<number>;
	private readonly capacity: number;
	private head = 0;
	private count = 0;

	constructor(capacity: number) {
		assert.ok(capacity > 0, 'stats history capacity must be positive');
		this.capacity = capacity;
		this.samples = new Array(capacity).fill(0);
	}

	push(value: number): void {
		assert.ok(Number.isFinite(value), 'stats history sample must be finite');
		if (this.count < this.capacity) {
			this.samples[(this.head + this.count) % this.capacity] = value;
			this.count += 1;
			return;
		}
		this.samples[this.head] = value;
		this.head = (this.head + 1) % this.capacity;
	}

	toArray(): Array<number> {
		const out = new Array<number>(this.count);
		for (let index = 0; index < this.count; index += 1) {
			out[index] = this.samples[(this.head + index) % this.capacity] ?? 0;
		}
		return out;
	}
}

export function useStatsForNerds({enabled = true}: UseStatsForNerdsOptions = {}): StatsForNerdsData {
	useMediaEngineVersion();
	const [gpuInfo, setGpuInfo] = useState<GpuInfo | null>(null);
	const [appMetrics, setAppMetrics] = useState<AppMetricsSnapshot | null>(null);
	const [desktopInfo, setDesktopInfo] = useState<DesktopInfo | null>(null);
	const heapHistoryRef = useRef<BoundedNumberHistory | null>(null);
	const cpuHistoryRef = useRef<BoundedNumberHistory | null>(null);
	if (!heapHistoryRef.current) heapHistoryRef.current = new BoundedNumberHistory(MAX_STATS_HISTORY_SAMPLES);
	if (!cpuHistoryRef.current) cpuHistoryRef.current = new BoundedNumberHistory(MAX_STATS_HISTORY_SAMPLES);
	useEffect(() => {
		if (!enabled) return;
		let cancelled = false;
		const electronAPI = getElectronAPI();
		if (electronAPI?.getGpuInfo) {
			void electronAPI.getGpuInfo().then((info) => {
				if (!cancelled) setGpuInfo(info);
			});
		}
		if (electronAPI?.getDesktopInfo) {
			void electronAPI.getDesktopInfo().then((info) => {
				if (!cancelled) setDesktopInfo(info);
			});
		}
		return () => {
			cancelled = true;
		};
	}, [enabled]);
	useEffect(() => {
		if (!enabled) return;
		let cancelled = false;
		const electronAPI = getElectronAPI();
		const mem = (performance as Performance & {memory?: {usedJSHeapSize: number}}).memory;
		if (!electronAPI?.getAppMetrics && !mem) return;
		const poll = (): void => {
			if (mem) {
				heapHistoryRef.current?.push(Math.round((mem.usedJSHeapSize / 1048576) * 10) / 10);
			}
			if (!electronAPI?.getAppMetrics) return;
			void electronAPI.getAppMetrics().then((metrics) => {
				if (cancelled) return;
				setAppMetrics(metrics);
				const mainProcess = metrics.processes.find((p) => p.type === 'Browser');
				if (!mainProcess) return;
				cpuHistoryRef.current?.push(mainProcess.cpu.percentCPUUsage);
			});
		};
		poll();
		const id = setInterval(poll, STATS_FOR_NERDS_POLL_INTERVAL_MS);
		return () => {
			cancelled = true;
			clearInterval(id);
		};
	}, [enabled]);
	const snapshot = collectStatsForNerdsSnapshot();
	return {
		...snapshot,
		appInfo: {
			appVersion: snapshot.appInfo.appVersion,
			electronVersion: desktopInfo?.electronVersion ?? null,
			chromiumVersion: desktopInfo?.chromeVersion ?? null,
			hardwareAccelerationEnabled:
				desktopInfo != null ? getCachedDesktopTroubleshootingSettings()?.disableHardwareAcceleration !== true : null,
			chromiumRuntime: desktopInfo?.chromiumRuntime ?? null,
		},
		gpu: gpuInfo,
		appMetrics,
		system: getSystemInfo(),
		heapHistory: heapHistoryRef.current.toArray(),
		cpuHistory: cpuHistoryRef.current.toArray(),
	};
}
