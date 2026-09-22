// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AppMetricsSnapshot} from '@app/features/platform/types/Electron';
import type {StatsForNerdsData} from '@app/features/voice/utils/VoiceStatsForNerdsPresenter';
import type {VoiceEngineV2PerTrackStats} from '@fluxer/voice_engine_v2';
import {renderToStaticMarkup} from 'react-dom/server';

export const VOICE_DEBUG_STATS_JSON_ELEMENT_ID = 'stats-json';
const VOICE_DEBUG_STATS_MAX_VALUE_CHARS = 4096;
const VOICE_DEBUG_STATS_MAX_RAW_JSON_CHARS = 262_144;
const VOICE_DEBUG_STATS_SPARKLINE_MAX_SAMPLES = 60;
const VOICE_DEBUG_STATS_SPARKLINE_WIDTH = 240;
const VOICE_DEBUG_STATS_SPARKLINE_HEIGHT = 48;
const VOICE_DEBUG_STATS_SPARKLINE_PADDING = 4;

function truncateStatsText(value: string, maxChars: number): string {
	if (value.length <= maxChars) return value;
	const omittedChars = value.length - maxChars;
	return `${value.slice(0, maxChars)}... [truncated ${omittedChars} chars]`;
}

function formatStatsValue(value: unknown): string {
	if (value === null || value === undefined) return 'n/a';
	if (typeof value === 'string') {
		return value === '' ? 'n/a' : truncateStatsText(value, VOICE_DEBUG_STATS_MAX_VALUE_CHARS);
	}
	if (typeof value === 'number') return Number.isFinite(value) ? String(value) : 'n/a';
	if (typeof value === 'boolean') return value ? 'true' : 'false';
	try {
		return truncateStatsText(JSON.stringify(value) ?? 'n/a', VOICE_DEBUG_STATS_MAX_VALUE_CHARS);
	} catch {
		return String(value);
	}
}

function formatHistory(samples: ReadonlyArray<number>): string {
	if (samples.length === 0) return 'n/a';
	return samples.map((sample) => formatStatsValue(sample)).join(', ');
}

function formatSparklineNumber(value: number): string {
	if (Number.isInteger(value)) return String(value);
	return String(Math.round(value * 100) / 100);
}

interface StatsTableProps {
	title: string;
	record: Record<string, unknown> | null;
}

function StatsTable({title, record}: StatsTableProps) {
	const entries = record ? Object.entries(record) : [];
	return (
		<section data-flx="voice.diagnostics.voice-debug-stats-html.stats-table.section">
			<h3 data-flx="voice.diagnostics.voice-debug-stats-html.stats-table.h3">{title}</h3>
			{entries.length > 0 ? (
				<table data-flx="voice.diagnostics.voice-debug-stats-html.stats-table.table">
					<tbody data-flx="voice.diagnostics.voice-debug-stats-html.stats-table.tbody">
						{entries.map(([key, value]) => (
							<tr key={key} data-flx="voice.diagnostics.voice-debug-stats-html.stats-table.tr">
								<td data-flx="voice.diagnostics.voice-debug-stats-html.stats-table.td">{key}</td>
								<td data-flx="voice.diagnostics.voice-debug-stats-html.stats-table.td--2">{formatStatsValue(value)}</td>
							</tr>
						))}
					</tbody>
				</table>
			) : (
				<p data-flx="voice.diagnostics.voice-debug-stats-html.stats-table.p">none</p>
			)}
		</section>
	);
}

interface SparklinePoints {
	points: string;
	min: number;
	max: number;
	current: number;
}

interface SparklineRowProps {
	label: string;
	unit: string;
	samples: ReadonlyArray<number>;
}

function getSparklineSamples(samples: ReadonlyArray<number>): Array<number> {
	const startIndex = Math.max(0, samples.length - VOICE_DEBUG_STATS_SPARKLINE_MAX_SAMPLES);
	const finiteSamples: Array<number> = [];
	for (let i = startIndex; i < samples.length; i += 1) {
		const sample = samples[i];
		if (Number.isFinite(sample)) finiteSamples.push(sample);
	}
	return finiteSamples;
}

function buildSparklinePoints(rawSamples: ReadonlyArray<number>): SparklinePoints | null {
	const samples = getSparklineSamples(rawSamples);
	if (samples.length === 0) return null;
	let min = samples[0];
	let max = samples[0];
	for (const sample of samples) {
		if (sample < min) min = sample;
		if (sample > max) max = sample;
	}
	const drawableWidth = VOICE_DEBUG_STATS_SPARKLINE_WIDTH - VOICE_DEBUG_STATS_SPARKLINE_PADDING * 2;
	const drawableHeight = VOICE_DEBUG_STATS_SPARKLINE_HEIGHT - VOICE_DEBUG_STATS_SPARKLINE_PADDING * 2;
	const range = max - min;
	const divisor = Math.max(1, samples.length - 1);
	const points = samples
		.map((sample, index) => {
			const x = VOICE_DEBUG_STATS_SPARKLINE_PADDING + (index / divisor) * drawableWidth;
			const normalized = range === 0 ? 0.5 : (sample - min) / range;
			const y = VOICE_DEBUG_STATS_SPARKLINE_HEIGHT - VOICE_DEBUG_STATS_SPARKLINE_PADDING - normalized * drawableHeight;
			return `${formatSparklineNumber(x)},${formatSparklineNumber(y)}`;
		})
		.join(' ');
	return {points, min, max, current: samples[samples.length - 1]};
}

function SparklineRow({label, unit, samples}: SparklineRowProps) {
	const sparkline = buildSparklinePoints(samples);
	const valueText = sparkline
		? `current ${formatSparklineNumber(sparkline.current)}${unit}; min ${formatSparklineNumber(
				sparkline.min,
			)}${unit}; max ${formatSparklineNumber(sparkline.max)}${unit}`
		: 'none';
	return (
		<tr data-flx="voice.diagnostics.voice-debug-stats-html.sparkline-row.tr">
			<td data-flx="voice.diagnostics.voice-debug-stats-html.sparkline-row.td">{label}</td>
			<td data-flx="voice.diagnostics.voice-debug-stats-html.sparkline-row.td--2">{valueText}</td>
			<td data-flx="voice.diagnostics.voice-debug-stats-html.sparkline-row.td--3">
				{sparkline ? (
					<svg
						width={VOICE_DEBUG_STATS_SPARKLINE_WIDTH}
						height={VOICE_DEBUG_STATS_SPARKLINE_HEIGHT}
						viewBox={`0 0 ${VOICE_DEBUG_STATS_SPARKLINE_WIDTH} ${VOICE_DEBUG_STATS_SPARKLINE_HEIGHT}`}
						role="img"
						aria-label={`${label} sparkline: ${valueText}`}
						data-flx="voice.diagnostics.voice-debug-stats-html.sparkline-row.img"
					>
						<title data-flx="voice.diagnostics.voice-debug-stats-html.sparkline-row.title">{`${label} sparkline`}</title>
						<rect
							x="0"
							y="0"
							width={VOICE_DEBUG_STATS_SPARKLINE_WIDTH}
							height={VOICE_DEBUG_STATS_SPARKLINE_HEIGHT}
							fill="white"
							stroke="black"
							strokeWidth="1"
							data-flx="voice.diagnostics.voice-debug-stats-html.sparkline-row.rect"
						/>
						<polyline
							points={sparkline.points}
							fill="none"
							stroke="black"
							strokeWidth="2"
							data-flx="voice.diagnostics.voice-debug-stats-html.sparkline-row.polyline"
						/>
					</svg>
				) : (
					'none'
				)}
			</td>
		</tr>
	);
}

function asStatsRecord(value: object | null | undefined): Record<string, unknown> | null {
	if (!value) return null;
	return value as Record<string, unknown>;
}

interface ProcessTableProps {
	metrics: AppMetricsSnapshot | null;
}

function ProcessTable({metrics}: ProcessTableProps) {
	if (!metrics) {
		return (
			<section data-flx="voice.diagnostics.voice-debug-stats-html.process-table.section">
				<h3 data-flx="voice.diagnostics.voice-debug-stats-html.process-table.h3">appMetrics</h3>
				<p data-flx="voice.diagnostics.voice-debug-stats-html.process-table.p">none</p>
			</section>
		);
	}
	return (
		<section data-flx="voice.diagnostics.voice-debug-stats-html.process-table.section--2">
			<h3 data-flx="voice.diagnostics.voice-debug-stats-html.process-table.h3--2">appMetrics</h3>
			<table data-flx="voice.diagnostics.voice-debug-stats-html.process-table.table">
				<thead data-flx="voice.diagnostics.voice-debug-stats-html.process-table.thead">
					<tr data-flx="voice.diagnostics.voice-debug-stats-html.process-table.tr">
						<th data-flx="voice.diagnostics.voice-debug-stats-html.process-table.th">type</th>
						<th data-flx="voice.diagnostics.voice-debug-stats-html.process-table.th--2">name</th>
						<th data-flx="voice.diagnostics.voice-debug-stats-html.process-table.th--3">pid</th>
						<th data-flx="voice.diagnostics.voice-debug-stats-html.process-table.th--4">cpuPercent</th>
						<th data-flx="voice.diagnostics.voice-debug-stats-html.process-table.th--5">workingSetKB</th>
						<th data-flx="voice.diagnostics.voice-debug-stats-html.process-table.th--6">peakWorkingSetKB</th>
					</tr>
				</thead>
				<tbody data-flx="voice.diagnostics.voice-debug-stats-html.process-table.tbody">
					{metrics.processes.map((process) => (
						<tr key={process.pid} data-flx="voice.diagnostics.voice-debug-stats-html.process-table.tr--2">
							<td data-flx="voice.diagnostics.voice-debug-stats-html.process-table.td">{process.type}</td>
							<td data-flx="voice.diagnostics.voice-debug-stats-html.process-table.td--2">{process.name ?? 'n/a'}</td>
							<td data-flx="voice.diagnostics.voice-debug-stats-html.process-table.td--3">{process.pid}</td>
							<td data-flx="voice.diagnostics.voice-debug-stats-html.process-table.td--4">
								{formatStatsValue(process.cpu.percentCPUUsage)}
							</td>
							<td data-flx="voice.diagnostics.voice-debug-stats-html.process-table.td--5">
								{formatStatsValue(process.memory.workingSetSize)}
							</td>
							<td data-flx="voice.diagnostics.voice-debug-stats-html.process-table.td--6">
								{formatStatsValue(process.memory.peakWorkingSetSize)}
							</td>
						</tr>
					))}
				</tbody>
			</table>
			<p data-flx="voice.diagnostics.voice-debug-stats-html.process-table.p--2">
				cpu: {metrics.cpuInfo.model} ({metrics.cpuInfo.cores} cores, {metrics.cpuInfo.physicalCores} physical), memory:{' '}
				{formatStatsValue(metrics.freeMemoryMB)} MB free of {formatStatsValue(metrics.totalMemoryMB)} MB
			</p>
		</section>
	);
}

interface TrackTablesProps {
	data: StatsForNerdsData;
}

function TrackTables({data}: TrackTablesProps) {
	const tracks: Array<[string, VoiceEngineV2PerTrackStats | null]> = [
		['track:localAudio', data.localAudio],
		['track:localVideo', data.localVideo],
		...data.localVideoLayers.map((layer, index): [string, VoiceEngineV2PerTrackStats | null] => [
			`track:localVideoLayers[${index}]`,
			layer,
		]),
		['track:localScreenShare', data.localScreenShare],
		['track:localScreenShareAudio', data.localScreenShareAudio],
		['track:remoteAudio', data.remoteAudio],
		['track:remoteVideo', data.remoteVideo],
		['track:remoteScreenShare', data.remoteScreenShare],
		['track:remoteScreenShareAudio', data.remoteScreenShareAudio],
	];
	return (
		<>
			{tracks.map(([title, track]) => (
				<StatsTable
					key={title}
					title={title}
					record={asStatsRecord(track)}
					data-flx="voice.diagnostics.voice-debug-stats-html.track-tables.stats-table"
				/>
			))}
		</>
	);
}

interface VoiceDebugStatsProps {
	data: StatsForNerdsData;
	generatedAtIso: string;
}

interface VoiceDebugStatsUnavailableProps {
	generatedAtIso: string;
	message: string;
}

function buildNetworkRecord(data: StatsForNerdsData): Record<string, unknown> {
	const {publisherTransport, subscriberTransport, ...network} = data.network;
	return network;
}

function buildHistoryRecord(data: StatsForNerdsData): Record<string, unknown> {
	return {
		heapUsedMB: formatHistory(data.heapHistory),
		mainProcessCpuPercent: formatHistory(data.cpuHistory),
		latencyMs: formatHistory(data.sparklines.latency),
		totalBitrateKbps: formatHistory(data.sparklines.bitrate),
		packetLossPercent: formatHistory(data.sparklines.packetLoss),
	};
}

function buildRawJson(data: StatsForNerdsData): string {
	return truncateStatsText(JSON.stringify(data, null, 2), VOICE_DEBUG_STATS_MAX_RAW_JSON_CHARS);
}

function SparklineTable({data}: VoiceDebugStatsProps) {
	return (
		<section data-flx="voice.diagnostics.voice-debug-stats-html.sparkline-table.section">
			<h3 data-flx="voice.diagnostics.voice-debug-stats-html.sparkline-table.h3">keyMetricSparklines</h3>
			<table data-flx="voice.diagnostics.voice-debug-stats-html.sparkline-table.table">
				<thead data-flx="voice.diagnostics.voice-debug-stats-html.sparkline-table.thead">
					<tr data-flx="voice.diagnostics.voice-debug-stats-html.sparkline-table.tr">
						<th data-flx="voice.diagnostics.voice-debug-stats-html.sparkline-table.th">metric</th>
						<th data-flx="voice.diagnostics.voice-debug-stats-html.sparkline-table.th--2">values</th>
						<th data-flx="voice.diagnostics.voice-debug-stats-html.sparkline-table.th--3">sparkline</th>
					</tr>
				</thead>
				<tbody data-flx="voice.diagnostics.voice-debug-stats-html.sparkline-table.tbody">
					<SparklineRow
						label="latencyMs"
						unit="ms"
						samples={data.sparklines.latency}
						data-flx="voice.diagnostics.voice-debug-stats-html.sparkline-table.sparkline-row"
					/>
					<SparklineRow
						label="totalBitrateKbps"
						unit="kbps"
						samples={data.sparklines.bitrate}
						data-flx="voice.diagnostics.voice-debug-stats-html.sparkline-table.sparkline-row--2"
					/>
					<SparklineRow
						label="packetLossPercent"
						unit="%"
						samples={data.sparklines.packetLoss}
						data-flx="voice.diagnostics.voice-debug-stats-html.sparkline-table.sparkline-row--3"
					/>
					<SparklineRow
						label="heapUsedMB"
						unit="MB"
						samples={data.heapHistory}
						data-flx="voice.diagnostics.voice-debug-stats-html.sparkline-table.sparkline-row--4"
					/>
					<SparklineRow
						label="mainProcessCpuPercent"
						unit="%"
						samples={data.cpuHistory}
						data-flx="voice.diagnostics.voice-debug-stats-html.sparkline-table.sparkline-row--5"
					/>
				</tbody>
			</table>
		</section>
	);
}

function VoiceDebugStats({data, generatedAtIso}: VoiceDebugStatsProps) {
	return (
		<div data-flx="voice.diagnostics.voice-debug-stats-html.voice-debug-stats.div">
			<p data-flx="voice.diagnostics.voice-debug-stats-html.voice-debug-stats.p">generated {generatedAtIso}</p>
			<StatsTable
				title="session"
				record={asStatsRecord(data.session)}
				data-flx="voice.diagnostics.voice-debug-stats-html.voice-debug-stats.stats-table"
			/>
			<StatsTable
				title="connection"
				record={asStatsRecord(data.connection)}
				data-flx="voice.diagnostics.voice-debug-stats-html.voice-debug-stats.stats-table--2"
			/>
			<StatsTable
				title="network"
				record={buildNetworkRecord(data)}
				data-flx="voice.diagnostics.voice-debug-stats-html.voice-debug-stats.stats-table--3"
			/>
			<StatsTable
				title="publisherTransport"
				record={asStatsRecord(data.network.publisherTransport)}
				data-flx="voice.diagnostics.voice-debug-stats-html.voice-debug-stats.stats-table--4"
			/>
			<StatsTable
				title="subscriberTransport"
				record={asStatsRecord(data.network.subscriberTransport)}
				data-flx="voice.diagnostics.voice-debug-stats-html.voice-debug-stats.stats-table--5"
			/>
			<TrackTables data={data} data-flx="voice.diagnostics.voice-debug-stats-html.voice-debug-stats.track-tables" />
			<StatsTable
				title="audioProcessing"
				record={asStatsRecord(data.audio)}
				data-flx="voice.diagnostics.voice-debug-stats-html.voice-debug-stats.stats-table--6"
			/>
			<StatsTable
				title="screenShareSettings"
				record={asStatsRecord(data.screenShareSettings)}
				data-flx="voice.diagnostics.voice-debug-stats-html.voice-debug-stats.stats-table--7"
			/>
			<StatsTable
				title="screenShareAudioCapture (native)"
				record={asStatsRecord(data.screenShareAudioCapture.nativeCapture)}
				data-flx="voice.diagnostics.voice-debug-stats-html.voice-debug-stats.stats-table--9"
			/>
			<StatsTable
				title="appInfo"
				record={asStatsRecord(data.appInfo)}
				data-flx="voice.diagnostics.voice-debug-stats-html.voice-debug-stats.stats-table--10"
			/>
			<StatsTable
				title="system"
				record={asStatsRecord(data.system)}
				data-flx="voice.diagnostics.voice-debug-stats-html.voice-debug-stats.stats-table--11"
			/>
			<StatsTable
				title="gpu"
				record={asStatsRecord(data.gpu)}
				data-flx="voice.diagnostics.voice-debug-stats-html.voice-debug-stats.stats-table--12"
			/>
			<ProcessTable
				metrics={data.appMetrics}
				data-flx="voice.diagnostics.voice-debug-stats-html.voice-debug-stats.process-table"
			/>
			<SparklineTable
				data={data}
				generatedAtIso={generatedAtIso}
				data-flx="voice.diagnostics.voice-debug-stats-html.voice-debug-stats.sparkline-table"
			/>
			<StatsTable
				title="history"
				record={buildHistoryRecord(data)}
				data-flx="voice.diagnostics.voice-debug-stats-html.voice-debug-stats.stats-table--13"
			/>
			<details data-flx="voice.diagnostics.voice-debug-stats-html.voice-debug-stats.details">
				<summary data-flx="voice.diagnostics.voice-debug-stats-html.voice-debug-stats.summary">raw JSON</summary>
				<pre
					id={VOICE_DEBUG_STATS_JSON_ELEMENT_ID}
					data-flx="voice.diagnostics.voice-debug-stats-html.voice-debug-stats.pre"
				>
					{buildRawJson(data)}
				</pre>
			</details>
		</div>
	);
}

function VoiceDebugStatsUnavailable({generatedAtIso, message}: VoiceDebugStatsUnavailableProps) {
	return (
		<div data-flx="voice.diagnostics.voice-debug-stats-html.voice-debug-stats-unavailable.div">
			<p data-flx="voice.diagnostics.voice-debug-stats-html.voice-debug-stats-unavailable.p">
				generated {generatedAtIso}
			</p>
			<p data-flx="voice.diagnostics.voice-debug-stats-html.voice-debug-stats-unavailable.p--2">{message}</p>
			<pre
				id={VOICE_DEBUG_STATS_JSON_ELEMENT_ID}
				data-flx="voice.diagnostics.voice-debug-stats-html.voice-debug-stats-unavailable.pre"
			>
				{'{}'}
			</pre>
		</div>
	);
}

export function renderVoiceDebugStatsHtml(data: StatsForNerdsData, generatedAtIso: string): string {
	return renderToStaticMarkup(
		<VoiceDebugStats
			data={data}
			generatedAtIso={generatedAtIso}
			data-flx="voice.diagnostics.voice-debug-stats-html.render-voice-debug-stats-html.voice-debug-stats"
		/>,
	);
}

export function renderVoiceDebugStatsUnavailableHtml(message: string, generatedAtIso: string): string {
	return renderToStaticMarkup(
		<VoiceDebugStatsUnavailable
			message={message}
			generatedAtIso={generatedAtIso}
			data-flx="voice.diagnostics.voice-debug-stats-html.render-voice-debug-stats-unavailable-html.voice-debug-stats-unavailable"
		/>,
	);
}
