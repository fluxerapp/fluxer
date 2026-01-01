/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {execFile} from 'node:child_process';
import {promisify} from 'node:util';
import {temporaryFile} from 'tempy';

const execFileAsync = promisify(execFile);

export interface FFprobeStream {
	codec_name?: string;
	codec_type?: string;
}

interface FFprobeFormat {
	format_name?: string;
	duration?: string;
	size?: string;
}

interface FFprobeResult {
	streams?: Array<FFprobeStream>;
	format?: FFprobeFormat;
}

const parseProbeOutput = (stdout: string): FFprobeResult => {
	const parsed = JSON.parse(stdout) as unknown;
	if (!parsed || typeof parsed !== 'object') {
		throw new Error('Invalid ffprobe output');
	}
	return parsed as FFprobeResult;
};

export const ffprobe = async (path: string): Promise<FFprobeResult> => {
	const {stdout} = await execFileAsync('ffprobe', [
		'-v',
		'quiet',
		'-print_format',
		'json',
		'-show_format',
		'-show_streams',
		path,
	]);
	return parseProbeOutput(stdout);
};

export const hasVideoStream = async (path: string): Promise<boolean> => {
	const probeResult = await ffprobe(path);
	return probeResult.streams?.some((stream) => stream.codec_type === 'video') ?? false;
};

export const createThumbnail = async (videoPath: string): Promise<string> => {
	const hasVideo = await hasVideoStream(videoPath);
	if (!hasVideo) {
		throw new Error('File does not contain a video stream');
	}
	const thumbnailPath = temporaryFile({extension: 'jpg'});
	await execFileAsync('ffmpeg', ['-i', videoPath, '-vf', 'select=eq(n\\,0)', '-vframes', '1', thumbnailPath]);
	return thumbnailPath;
};
