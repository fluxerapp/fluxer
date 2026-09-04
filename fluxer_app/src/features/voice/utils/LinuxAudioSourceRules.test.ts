// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VirtmicNode} from '@app/types/electron.d';
import {describe, expect, it} from 'vitest';
import {
	filterRoutableLinuxAudioSources,
	getLinuxAudioSourceDisplayName,
	LINUX_AUDIO_DISPLAY_NAME_PATTERN_KEY,
	LINUX_AUDIO_PRESERVE_VOLATILE_IDENTITY_PATTERN_KEY,
	LINUX_AUDIO_TARGET_OBJECTS_PATTERN_KEY,
	linuxAudioSourceItemKey,
	mapLinuxAudioNodeToItems,
	toNativeLinuxAudioPattern,
	toNativeLinuxAudioPatterns,
	uniqueLinuxAudioSourceItems,
} from './LinuxAudioSourceRules';

function node(overrides: VirtmicNode): VirtmicNode {
	return overrides;
}

describe('LinuxAudioSourceRules', () => {
	it('hides playback devices unless device selection is enabled', () => {
		const sink = node({
			'media.class': 'Audio/Sink',
			'application.name': 'WirePlumber',
			'node.name': 'alsa_output.pci-0000_00_1f.3.analog-stereo',
			'node.description': 'Built-in audio analog stereo',
			'object.serial': '42',
		});
		expect(mapLinuxAudioNodeToItems(sink, {granular: false, deviceSelect: false, ignoreVirtual: false})).toEqual([]);
		expect(mapLinuxAudioNodeToItems(sink, {granular: false, deviceSelect: true, ignoreVirtual: false})).toEqual([
			{
				name: 'Built-in audio analog stereo',
				value: {
					[LINUX_AUDIO_DISPLAY_NAME_PATTERN_KEY]: 'Built-in audio analog stereo',
					[LINUX_AUDIO_TARGET_OBJECTS_PATTERN_KEY]: '42\nalsa_output.pci-0000_00_1f.3.analog-stereo',
				},
			},
		]);
	});
	it('maps app playback streams to app and granular selectors', () => {
		const stream = node({
			'media.class': 'Stream/Output/Audio',
			'application.name': 'Firefox',
			'application.process.id': '1234',
			'media.name': 'Web audio',
		});
		expect(mapLinuxAudioNodeToItems(stream, {granular: true, deviceSelect: false, ignoreVirtual: false})).toEqual([
			{name: 'Firefox', value: {'application.name': 'Firefox'}},
			{
				name: 'Firefox (1234)',
				value: {
					'application.name': 'Firefox',
					'application.process.id': '1234',
					[LINUX_AUDIO_PRESERVE_VOLATILE_IDENTITY_PATTERN_KEY]: 'true',
				},
			},
			{
				name: 'Firefox [Web audio]',
				value: {'application.name': 'Firefox', 'media.name': 'Web audio'},
			},
			{
				name: 'Firefox [Stream/Output/Audio]',
				value: {'application.name': 'Firefox', 'media.class': 'Stream/Output/Audio'},
			},
		]);
	});
	it('falls back to stable PipeWire stream identity when app pid is unavailable', () => {
		const stream = node({
			'media.class': 'Stream/Output/Audio',
			'application.name': 'Chromium',
			'client.id': '77',
			'object.serial': '88',
		});
		expect(mapLinuxAudioNodeToItems(stream, {granular: false, deviceSelect: false, ignoreVirtual: false})).toEqual([
			{name: 'Chromium', value: {'application.name': 'Chromium'}},
		]);
	});
	it('filters virtual, input, video, and midi nodes out of picker entries', () => {
		const options = {granular: true, deviceSelect: true, ignoreVirtual: true};
		expect(
			mapLinuxAudioNodeToItems(node({'media.class': 'Stream/Output/Audio', 'node.virtual': 'true'}), options),
		).toEqual([]);
		expect(
			mapLinuxAudioNodeToItems(node({'media.class': 'Stream/Input/Audio', 'application.name': 'Recorder'}), options),
		).toEqual([]);
		expect(
			mapLinuxAudioNodeToItems(node({'media.class': 'Video/Source', 'application.name': 'Camera'}), options),
		).toEqual([]);
		expect(
			mapLinuxAudioNodeToItems(node({'media.class': 'Midi/Bridge', 'application.name': 'Synth'}), options),
		).toEqual([]);
	});
	it('strips display-only metadata before sending rules to native routing', () => {
		const pattern = {
			[LINUX_AUDIO_DISPLAY_NAME_PATTERN_KEY]: 'Built-in speakers',
			[LINUX_AUDIO_TARGET_OBJECTS_PATTERN_KEY]: '42\nalsa_output.foo',
		};
		expect(toNativeLinuxAudioPattern(pattern)).toEqual({
			[LINUX_AUDIO_TARGET_OBJECTS_PATTERN_KEY]: '42\nalsa_output.foo',
		});
		expect(toNativeLinuxAudioPattern({[LINUX_AUDIO_DISPLAY_NAME_PATTERN_KEY]: 'Built-in speakers'})).toBeNull();
		expect(toNativeLinuxAudioPatterns([pattern, {[LINUX_AUDIO_DISPLAY_NAME_PATTERN_KEY]: 'Display only'}])).toEqual([
			{[LINUX_AUDIO_TARGET_OBJECTS_PATTERN_KEY]: '42\nalsa_output.foo'},
		]);
	});
	it('normalizes legacy app selectors by stripping volatile PipeWire identity before native routing', () => {
		expect(
			toNativeLinuxAudioPattern({
				'application.name': 'Chromium',
				'application.process.id': '1935',
				'client.id': '77',
				'object.serial': '88',
			}),
		).toEqual({'application.name': 'Chromium'});
		expect(
			toNativeLinuxAudioPattern({
				'application.name': 'Firefox',
				'application.process.id': '1234',
				'media.name': 'Web audio',
			}),
		).toEqual({'application.name': 'Firefox', 'media.name': 'Web audio'});
		expect(
			toNativeLinuxAudioPattern({
				'application.name': 'Firefox',
				'application.process.id': '1234',
				[LINUX_AUDIO_PRESERVE_VOLATILE_IDENTITY_PATTERN_KEY]: 'true',
			}),
		).toEqual({'application.name': 'Firefox', 'application.process.id': '1234'});
	});
	it('summarizes selected synthetic device patterns by display name', () => {
		expect(
			getLinuxAudioSourceDisplayName({
				[LINUX_AUDIO_DISPLAY_NAME_PATTERN_KEY]: 'Built-in speakers',
				[LINUX_AUDIO_TARGET_OBJECTS_PATTERN_KEY]: '42',
			}),
		).toBe('Built-in speakers');
	});
	it('keeps only the selections the capture layer can still match on', () => {
		const routable = {'application.name': 'Firefox'};
		const displayOnly = {[LINUX_AUDIO_DISPLAY_NAME_PATTERN_KEY]: 'Firefox'};
		const volatileOnly = {'application.process.id': '1234'};
		expect(filterRoutableLinuxAudioSources([routable, displayOnly, volatileOnly])).toEqual([routable, volatileOnly]);
		expect(filterRoutableLinuxAudioSources([{...routable, [LINUX_AUDIO_DISPLAY_NAME_PATTERN_KEY]: 'Firefox'}])).toEqual(
			[{...routable, [LINUX_AUDIO_DISPLAY_NAME_PATTERN_KEY]: 'Firefox'}],
		);
		expect(filterRoutableLinuxAudioSources([])).toEqual([]);
	});
	it('deduplicates identical item values without hiding same-named different selectors', () => {
		const duplicate = {name: 'Firefox', value: {'application.name': 'Firefox'}};
		const pidSpecific = {name: 'Firefox', value: {'application.name': 'Firefox', 'application.process.id': '1234'}};
		expect(uniqueLinuxAudioSourceItems([duplicate, {...duplicate}, pidSpecific]).map(linuxAudioSourceItemKey)).toEqual([
			linuxAudioSourceItemKey(duplicate),
			linuxAudioSourceItemKey(pidSpecific),
		]);
	});
});
