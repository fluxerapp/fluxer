// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VirtmicNode} from '@app/types/electron.d';

export const LINUX_AUDIO_TARGET_OBJECTS_PATTERN_KEY = 'fluxer.target.objects';
export const LINUX_AUDIO_DISPLAY_PATTERN_PREFIX = 'fluxer.display.';
export const LINUX_AUDIO_DISPLAY_NAME_PATTERN_KEY = 'fluxer.display.name';
export const LINUX_AUDIO_PRESERVE_VOLATILE_IDENTITY_PATTERN_KEY = 'fluxer.display.preserveVolatileIdentity';

export interface LinuxAudioSourceItem {
	name: string;
	value: VirtmicNode;
}

export interface LinuxAudioSourceFilterOptions {
	granular: boolean;
	deviceSelect: boolean;
	ignoreVirtual: boolean;
}

export function linuxAudioSourceItemKey(item: LinuxAudioSourceItem): string {
	return JSON.stringify([item.name, item.value]);
}

function firstPresent(node: VirtmicNode, keys: ReadonlyArray<string>): string | undefined {
	for (const key of keys) {
		const value = node[key];
		if (value) return value;
	}
	return undefined;
}

export function getLinuxAudioSourceDisplayName(source: VirtmicNode | undefined): string | undefined {
	if (!source) return undefined;
	return source['application.name'] ?? source[LINUX_AUDIO_DISPLAY_NAME_PATTERN_KEY] ?? source['node.name'];
}

function uniqueValues(values: ReadonlyArray<string | undefined>): Array<string> {
	const seen = new Set<string>();
	const result: Array<string> = [];
	for (const value of values) {
		if (!value || seen.has(value)) continue;
		seen.add(value);
		result.push(value);
	}
	return result;
}

function buildPlaybackStreamPattern(appName: string): VirtmicNode {
	return {'application.name': appName};
}

export function buildLinuxAudioDeviceTargetPattern(node: VirtmicNode, displayName: string): VirtmicNode | null {
	const targetObjects = uniqueValues([node['object.serial'], node['node.name']]);
	if (targetObjects.length === 0) return null;
	return {
		[LINUX_AUDIO_TARGET_OBJECTS_PATTERN_KEY]: targetObjects.join('\n'),
		[LINUX_AUDIO_DISPLAY_NAME_PATTERN_KEY]: displayName,
	};
}

export function mapLinuxAudioNodeToItems(
	node: VirtmicNode,
	options: LinuxAudioSourceFilterOptions,
): Array<LinuxAudioSourceItem> {
	const mediaClass = node['media.class'];
	if (mediaClass?.includes('Video') || mediaClass?.includes('Midi')) {
		return [];
	}
	if (options.ignoreVirtual && node['node.virtual'] === 'true') {
		return [];
	}
	const prettyName = firstPresent(node, [
		'application.name',
		'node.description',
		'node.nick',
		'media.title',
		'media.name',
		'node.name',
	]);
	if (mediaClass === 'Audio/Sink') {
		if (!options.deviceSelect) return [];
		const deviceName = firstPresent(node, ['node.description', 'node.nick', 'node.name', 'object.serial']);
		if (!deviceName) return [];
		const value = buildLinuxAudioDeviceTargetPattern(node, deviceName);
		return value ? [{name: deviceName, value}] : [];
	}
	if (mediaClass && mediaClass !== 'Stream/Output/Audio') {
		return [];
	}
	if (!options.deviceSelect && node['device.id']) {
		return [];
	}
	const items: Array<LinuxAudioSourceItem> = [];
	const appName = node['application.name'];
	if (appName) {
		items.push({name: prettyName ?? appName, value: buildPlaybackStreamPattern(appName)});
	}
	if (!options.granular) {
		return items;
	}
	const rawName = node['node.name'];
	if (!appName && rawName) {
		items.push({name: prettyName ?? rawName, value: {'node.name': rawName}});
	}
	const binary = node['application.process.binary'];
	if (!appName && binary) {
		items.push({name: prettyName ?? binary, value: {'application.process.binary': binary}});
	}
	const pid = node['application.process.id'] ?? node['pipewire.sec.pid'];
	const first = items[0];
	if (pid && first && first.value['application.process.id'] !== pid) {
		items.push({
			name: `${first.name} (${pid})`,
			value: {
				...(first.value as VirtmicNode),
				'application.process.id': pid,
				[LINUX_AUDIO_PRESERVE_VOLATILE_IDENTITY_PATTERN_KEY]: 'true',
			},
		});
	}
	const mediaName = node['media.name'];
	if (mediaName && first) {
		items.push({
			name: `${first.name} [${mediaName}]`,
			value: {...(first.value as VirtmicNode), 'media.name': mediaName},
		});
	}
	if (mediaClass && first) {
		items.push({
			name: `${first.name} [${mediaClass}]`,
			value: {...(first.value as VirtmicNode), 'media.class': mediaClass},
		});
	}
	return items;
}

export function uniqueLinuxAudioSourceItems(items: Array<LinuxAudioSourceItem>): Array<LinuxAudioSourceItem> {
	const seen = new Set<string>();
	const result: Array<LinuxAudioSourceItem> = [];
	for (const item of items) {
		const key = linuxAudioSourceItemKey(item);
		if (!seen.has(key)) {
			seen.add(key);
			result.push(item);
		}
	}
	return result;
}

const LINUX_AUDIO_VOLATILE_PLAYBACK_PATTERN_KEYS = new Set([
	'application.process.id',
	'pipewire.sec.pid',
	'client.id',
	'object.serial',
]);

function removeLegacyVolatilePlaybackIdentity(pattern: VirtmicNode, nativePattern: VirtmicNode): void {
	if (pattern[LINUX_AUDIO_PRESERVE_VOLATILE_IDENTITY_PATTERN_KEY] === 'true') return;
	if (!nativePattern['application.name']) return;
	if (LINUX_AUDIO_TARGET_OBJECTS_PATTERN_KEY in nativePattern) return;
	for (const key of LINUX_AUDIO_VOLATILE_PLAYBACK_PATTERN_KEYS) {
		delete nativePattern[key];
	}
}

export function toNativeLinuxAudioPattern(pattern: VirtmicNode): VirtmicNode | null {
	const next: VirtmicNode = {};
	for (const [key, value] of Object.entries(pattern)) {
		if (key.startsWith(LINUX_AUDIO_DISPLAY_PATTERN_PREFIX)) continue;
		next[key] = value;
	}
	removeLegacyVolatilePlaybackIdentity(pattern, next);
	return Object.keys(next).length > 0 ? next : null;
}

export function toNativeLinuxAudioPatterns(patterns: Array<VirtmicNode>): Array<VirtmicNode> {
	return patterns.flatMap((pattern) => {
		const next = toNativeLinuxAudioPattern(pattern);
		return next ? [next] : [];
	});
}

export function filterRoutableLinuxAudioSources(patterns: ReadonlyArray<VirtmicNode>): Array<VirtmicNode> {
	return patterns.filter((pattern) => toNativeLinuxAudioPattern(pattern) != null);
}
