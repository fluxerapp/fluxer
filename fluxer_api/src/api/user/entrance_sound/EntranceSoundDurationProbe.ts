// SPDX-License-Identifier: AGPL-3.0-or-later

import {execFile} from 'node:child_process';
import fs from 'node:fs/promises';
import {promisify} from 'node:util';
import {Logger} from '@app/api/Logger';
import {isJsonRecord, parseJsonWithGuard} from '@app/api/utils/JsonBoundaryUtils';
import type {EntranceSoundExtension} from '@fluxer/constants/src/EntranceSoundConstants';
import {temporaryFile} from 'tempy';

const execFilePromise = promisify(execFile);
const FFPROBE_TIMEOUT_MS = 5_000;
const FFPROBE_MAX_BUFFER_BYTES = 512 * 1024;

interface FfprobeFrame {
	duration_time?: string;
	pkt_duration_time?: string;
}

interface FfprobeStream {
	duration?: string;
}

interface FfprobeOutput {
	frames?: Array<FfprobeFrame>;
	streams?: Array<FfprobeStream>;
	format?: {
		duration?: string;
	};
}

function isFfprobeFrame(value: unknown): value is FfprobeFrame {
	return (
		isJsonRecord(value) &&
		(value.duration_time === undefined || typeof value.duration_time === 'string') &&
		(value.pkt_duration_time === undefined || typeof value.pkt_duration_time === 'string')
	);
}

function isFfprobeStream(value: unknown): value is FfprobeStream {
	return isJsonRecord(value) && (value.duration === undefined || typeof value.duration === 'string');
}

function isFfprobeOutput(value: unknown): value is FfprobeOutput {
	if (!isJsonRecord(value)) return false;
	const format = value.format;
	return (
		(value.frames === undefined || (Array.isArray(value.frames) && value.frames.every(isFfprobeFrame))) &&
		(value.streams === undefined || (Array.isArray(value.streams) && value.streams.every(isFfprobeStream))) &&
		(format === undefined ||
			(isJsonRecord(format) && (format.duration === undefined || typeof format.duration === 'string')))
	);
}

export async function resolveEntranceSoundDurationMs(params: {
	bytes: Buffer;
	extension: EntranceSoundExtension;
	metadataDurationSeconds: number | null;
}): Promise<number | null> {
	const preciseDurationSeconds = await probeEntranceSoundDurationSeconds(params.bytes, params.extension).catch(
		(error) => {
			Logger.warn(
				{error, extension: params.extension, sizeBytes: params.bytes.length},
				'Failed to probe precise entrance sound duration; falling back to media metadata duration',
			);
			return null;
		},
	);
	const durationSeconds = preciseDurationSeconds ?? normalizeDurationSeconds(params.metadataDurationSeconds);
	if (durationSeconds == null) {
		return null;
	}
	return Math.round(durationSeconds * 1000);
}

async function probeEntranceSoundDurationSeconds(
	bytes: Buffer,
	extension: EntranceSoundExtension,
): Promise<number | null> {
	const inputPath = temporaryFile({extension});
	try {
		await fs.writeFile(inputPath, bytes);
		const {stdout} = await execFilePromise(
			'ffprobe',
			[
				'-v',
				'error',
				'-select_streams',
				'a:0',
				'-show_frames',
				'-show_entries',
				'frame=duration_time,pkt_duration_time:stream=duration:format=duration',
				'-of',
				'json',
				inputPath,
			],
			{
				timeout: FFPROBE_TIMEOUT_MS,
				maxBuffer: FFPROBE_MAX_BUFFER_BYTES,
			},
		);
		const output = parseJsonWithGuard(stdout.toString(), isFfprobeOutput);
		return output ? durationSecondsFromFfprobe(output) : null;
	} finally {
		await fs.unlink(inputPath).catch(() => {});
	}
}

function durationSecondsFromFfprobe(output: FfprobeOutput): number | null {
	const frameDurationSeconds = sumFrameDurationSeconds(output.frames);
	if (frameDurationSeconds != null) {
		return frameDurationSeconds;
	}
	const streamDurationSeconds = firstDurationSeconds(output.streams?.map((stream) => stream.duration));
	if (streamDurationSeconds != null) {
		return streamDurationSeconds;
	}
	return normalizeDurationSeconds(output.format?.duration ?? null);
}

function sumFrameDurationSeconds(frames: ReadonlyArray<FfprobeFrame> | undefined): number | null {
	if (!frames || frames.length === 0) {
		return null;
	}
	let total = 0;
	let counted = 0;
	for (const frame of frames) {
		const duration = normalizeDurationSeconds(frame.duration_time ?? frame.pkt_duration_time ?? null);
		if (duration == null) continue;
		total += duration;
		counted += 1;
	}
	return counted > 0 ? total : null;
}

function firstDurationSeconds(values: ReadonlyArray<string | undefined> | undefined): number | null {
	if (!values) {
		return null;
	}
	for (const value of values) {
		const duration = normalizeDurationSeconds(value ?? null);
		if (duration != null) {
			return duration;
		}
	}
	return null;
}

function normalizeDurationSeconds(value: string | number | null | undefined): number | null {
	if (value == null) {
		return null;
	}
	const duration = typeof value === 'number' ? value : Number.parseFloat(value);
	if (!Number.isFinite(duration) || duration <= 0) {
		return null;
	}
	return duration;
}
