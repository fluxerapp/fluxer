// SPDX-License-Identifier: AGPL-3.0-or-later

import {execFile} from 'node:child_process';
import {promisify} from 'node:util';
import type {RpcActivityPayload} from '@electron/main/rpc/RpcTypes';

const execFileAsync = promisify(execFile);
const COMMAND_TIMEOUT_MS = 1500;

interface MprisMetadata {
	artUrl?: string;
	title?: string;
	artist?: string;
}

async function runCommand(command: string, args: Array<string>): Promise<string | null> {
	try {
		const result = await execFileAsync(command, args, {
			encoding: 'utf8',
			timeout: COMMAND_TIMEOUT_MS,
			windowsHide: true,
		});
		return result.stdout.trim();
	} catch {
		return null;
	}
}

function normalizeText(value: string | undefined): string | null {
	const trimmed = value?.trim();
	return trimmed ? trimmed.toLocaleLowerCase() : null;
}

function isStaticAssetKey(value: string | undefined): boolean {
	if (!value) return false;
	return !value.includes('://') && !value.startsWith('mp:') && !value.startsWith('spotify:');
}

function isMusicLikeActivity(activity: RpcActivityPayload): boolean {
	return Boolean(activity.details?.trim() && activity.state?.trim());
}

function metadataMatchesActivity(activity: RpcActivityPayload, metadata: MprisMetadata): boolean {
	const activityTitle = normalizeText(activity.details ?? activity.name);
	const activityArtist = normalizeText(activity.state);
	const metadataTitle = normalizeText(metadata.title);
	const metadataArtist = normalizeText(metadata.artist);
	return Boolean(activityTitle && activityArtist && metadataTitle && metadataArtist) &&
		activityTitle === metadataTitle &&
		activityArtist === metadataArtist;
}

function parseBusctlMetadata(jsonText: string): MprisMetadata | null {
	try {
		const parsed = JSON.parse(jsonText) as {
			type?: string;
			data?: Record<string, {type?: string; data?: unknown}>;
		};
		if (parsed.type !== 'a{sv}' || !parsed.data) return null;
		const artUrl = typeof parsed.data['mpris:artUrl']?.data === 'string' ? parsed.data['mpris:artUrl'].data : undefined;
		const title = typeof parsed.data['xesam:title']?.data === 'string' ? parsed.data['xesam:title'].data : undefined;
		const artistData = parsed.data['xesam:artist']?.data;
		const artist = Array.isArray(artistData) && typeof artistData[0] === 'string' ? artistData[0] : undefined;
		if (!artUrl) return null;
		return {artUrl, title, artist};
	} catch {
		return null;
	}
}

async function listMprisPlayers(): Promise<Array<string>> {
	const output = await runCommand('gdbus', [
		'call',
		'--session',
		'--dest',
		'org.freedesktop.DBus',
		'--object-path',
		'/org/freedesktop/DBus',
		'--method',
		'org.freedesktop.DBus.ListNames',
	]);
	if (!output) return [];
	return Array.from(output.matchAll(/org\.mpris\.MediaPlayer2\.[^"' ,)]+/g), (match) => match[0]);
}

async function readPlaybackStatus(player: string): Promise<string | null> {
	const output = await runCommand('busctl', [
		'--user',
		'--json=short',
		'get-property',
		player,
		'/org/mpris/MediaPlayer2',
		'org.mpris.MediaPlayer2.Player',
		'PlaybackStatus',
	]);
	if (!output) return null;
	try {
		const parsed = JSON.parse(output) as {type?: string; data?: unknown};
		return parsed.type === 's' && typeof parsed.data === 'string' ? parsed.data : null;
	} catch {
		return null;
	}
}

async function readMetadata(player: string): Promise<MprisMetadata | null> {
	const output = await runCommand('busctl', [
		'--user',
		'--json=short',
		'get-property',
		player,
		'/org/mpris/MediaPlayer2',
		'org.mpris.MediaPlayer2.Player',
		'Metadata',
	]);
	return output ? parseBusctlMetadata(output) : null;
}

export async function resolveMprisCoverArtUrl(activity: RpcActivityPayload): Promise<string | undefined> {
	if (process.platform !== 'linux') return undefined;
	if (!isMusicLikeActivity(activity)) return undefined;
	if (!isStaticAssetKey(activity.assets?.large_image)) return undefined;

	const players = await listMprisPlayers();
	for (const player of players) {
		const playbackStatus = await readPlaybackStatus(player);
		if (playbackStatus !== 'Playing' && playbackStatus !== 'Paused') continue;
		const metadata = await readMetadata(player);
		if (!metadata?.artUrl) continue;
		if (metadataMatchesActivity(activity, metadata)) {
			return metadata.artUrl;
		}
	}
	return undefined;
}

export const __testables__ = {
	isStaticAssetKey,
	isMusicLikeActivity,
	metadataMatchesActivity,
	parseBusctlMetadata,
};
