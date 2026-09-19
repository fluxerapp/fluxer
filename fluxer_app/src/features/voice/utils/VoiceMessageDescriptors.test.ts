// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {selectLocalParticipantControlsViewState} from '@app/features/voice/components/voice_connection_status/LocalParticipantControlsStateMachine';
import {
	resolveScreenShareDeliveryNoticeTone,
	type ScreenShareDeliveryNotice,
	type ScreenShareDeliveryNoticeTone,
} from '@app/features/voice/engine/ScreenShareUnderperformance';
import type {ScreenShareDeliveryPlan} from '@app/features/voice/state/ScreenShareDelivery';
import type {ScreenShareTarget} from '@app/features/voice/utils/ScreenShareOptions';
import {type I18n, type MessageDescriptor, setupI18n} from '@lingui/core';
import {describe, expect, it, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => ({msg: (descriptor: MessageDescriptor) => descriptor}));

vi.mock('@app/features/app/config/Config', () => ({
	default: {
		PUBLIC_BUILD_VERSION: 'test',
		PUBLIC_RELEASE_CHANNEL: 'canary',
		PUBLIC_BOOTSTRAP_API_ENDPOINT: 'https://example.invalid',
		PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT: 'https://example.invalid',
	},
}));

const {
	formatScreenShareDeliveryNotice,
	formatScreenShareDeliveryStatus,
	formatScreenShareResolutionLabel,
	SCREEN_SHARE_SETTINGS_ADJUSTED_DESCRIPTOR,
	SCREEN_SHARE_STATUS_ASKED_FOR_DESCRIPTOR,
	SCREEN_SHARE_STATUS_SENDING_DESCRIPTOR,
	SCREEN_SHARE_STATUS_SOURCE_RATE_DESCRIPTOR,
	SCREEN_SHARE_STATUS_SOURCE_RESOLUTION_DESCRIPTOR,
	SCREEN_SHARE_TRY_FULL_QUALITY_AGAIN_DESCRIPTOR,
} = await import('@app/features/voice/utils/VoiceMessageDescriptors');

const icu = setupI18n({locale: 'en', messages: {en: {}}});

type Values = Record<string, unknown> | undefined;

function fakeI18n(render: (descriptor: MessageDescriptor, values: Values) => string): I18n {
	return {
		locale: 'pseudo',
		_: (descriptor: MessageDescriptor, values?: Values) => render(descriptor, values),
	} as unknown as I18n;
}

function renderIcu(descriptor: MessageDescriptor, values: Values): string {
	const message = descriptor.message;
	if (typeof message !== 'string') {
		throw new Error(`descriptor ${JSON.stringify(descriptor)} has no message`);
	}
	return icu._({id: message, message}, values);
}

const pseudoI18n = fakeI18n((descriptor, values) => `⟦${renderIcu(descriptor, values)}⟧`);

function captureNoticeDescriptor(notice: ScreenShareDeliveryNotice): {descriptor: MessageDescriptor; values: Values} {
	const calls: Array<{descriptor: MessageDescriptor; values: Values}> = [];
	formatScreenShareDeliveryNotice(
		fakeI18n((descriptor, values) => {
			calls.push({descriptor, values});
			return '';
		}),
		notice,
	);
	expect(calls).toHaveLength(1);
	return calls[0];
}

const NOTICES = {
	'device-framerate': {kind: 'device-framerate', width: 1920, height: 1080, frameRate: 15},
	'device-resolution': {kind: 'device-resolution', width: 1280, height: 720, frameRate: 60},
	'device-crossover': {
		kind: 'device-crossover',
		width: 852,
		height: 480,
		frameRate: 30,
		targetWidth: 1920,
		targetHeight: 1080,
		targetFrameRate: 60,
	},
	'codec-switched': {kind: 'codec-switched', codec: 'vp8', previousCodec: 'vp9'},
	'device-short': {kind: 'device-short', width: 2714, height: 762, frameRate: 15, deliveredFrameRate: 8.98},
	'connection-short': {kind: 'connection-short', width: 1280, height: 720, frameRate: 30, deliveredFrameRate: 7.5},
	'browser-resolution': {
		kind: 'browser-resolution',
		width: 1920,
		height: 1080,
		deliveredWidth: 1280,
		deliveredHeight: 720,
	},
	'capture-resolution': {
		kind: 'capture-resolution',
		width: 1280,
		height: 720,
		deliveredWidth: 854,
		deliveredHeight: 480,
	},
	'started-lower': {
		kind: 'started-lower',
		width: 1280,
		height: 720,
		frameRate: 30,
		targetWidth: 2560,
		targetHeight: 1440,
		targetFrameRate: 60,
	},
	'pinned-codec-short': {kind: 'pinned-codec-short', codec: 'h265'},
	'encoder-recovered': {kind: 'encoder-recovered', codec: 'h264'},
	'codec-viewer': {kind: 'codec-viewer', codec: 'vp8', pinnedCodec: 'av1'},
	'codec-device': {kind: 'codec-device', codec: 'vp9', pinnedCodec: 'av1'},
	'apply-failed': {kind: 'apply-failed'},
} satisfies {[Kind in ScreenShareDeliveryNotice['kind']]: Extract<ScreenShareDeliveryNotice, {kind: Kind}>};

const EXPECTED: {[Kind in ScreenShareDeliveryNotice['kind']]: string} = {
	'device-framerate': 'Your device could not keep up, so your stream now sends 15 FPS to keep 1080p sharp.',
	'device-resolution': 'Your device could not keep up, so your stream now sends 720p to keep 60 FPS smooth.',
	'device-crossover':
		'Your device could not keep up, so your stream now sends 480p at 30 FPS instead of 1080p at 60 FPS.',
	'codec-switched':
		'Your stream switched to VP8 because VP9 could not keep up on this device. Viewers may see a short pause.',
	'device-short':
		'Your device cannot keep up. Your stream is set to 15 FPS at 2714×762 and viewers are getting about 9 FPS.',
	'connection-short':
		'Your connection cannot keep up. Your stream is set to 30 FPS at 720p and viewers are getting about 8 FPS.',
	'browser-resolution': 'The browser is sending 720p instead of 1080p.',
	'capture-resolution': 'Your capture device is sending 480p instead of 720p.',
	'started-lower':
		'This device could not keep up with 1440p at 60 FPS last time, so your stream started at 720p at 30 FPS.',
	'pinned-codec-short':
		'H.265 (HEVC) cannot keep up on this device. Set the codec to Automatic in Advanced settings to allow a faster one.',
	'encoder-recovered': 'Your video encoder stopped, so your stream switched to H.264.',
	'codec-viewer': 'A viewer cannot play AV1, so your stream uses VP8.',
	'codec-device': 'AV1 is not available on this device, so your stream uses VP9.',
	'apply-failed': 'Some stream settings could not be applied to your live stream.',
};

const TEMPLATES: {[Kind in ScreenShareDeliveryNotice['kind']]: string} = {
	'device-framerate':
		'Your device could not keep up, so your stream now sends {frameRate} FPS to keep {resolution} sharp.',
	'device-resolution':
		'Your device could not keep up, so your stream now sends {resolution} to keep {frameRate} FPS smooth.',
	'device-crossover':
		'Your device could not keep up, so your stream now sends {resolution} at {frameRate} FPS instead of {targetResolution} at {targetFrameRate} FPS.',
	'codec-switched':
		'Your stream switched to {codec} because {previousCodec} could not keep up on this device. Viewers may see a short pause.',
	'device-short':
		'Your device cannot keep up. Your stream is set to {frameRate} FPS at {resolution} and viewers are getting about {deliveredFrameRate} FPS.',
	'connection-short':
		'Your connection cannot keep up. Your stream is set to {frameRate} FPS at {resolution} and viewers are getting about {deliveredFrameRate} FPS.',
	'browser-resolution': 'The browser is sending {deliveredResolution} instead of {resolution}.',
	'capture-resolution': 'Your capture device is sending {deliveredResolution} instead of {resolution}.',
	'started-lower':
		'This device could not keep up with {targetResolution} at {targetFrameRate} FPS last time, so your stream started at {resolution} at {frameRate} FPS.',
	'pinned-codec-short':
		'{codec} cannot keep up on this device. Set the codec to Automatic in Advanced settings to allow a faster one.',
	'encoder-recovered': 'Your video encoder stopped, so your stream switched to {codec}.',
	'codec-viewer': 'A viewer cannot play {pinnedCodec}, so your stream uses {codec}.',
	'codec-device': '{pinnedCodec} is not available on this device, so your stream uses {codec}.',
	'apply-failed': 'Some stream settings could not be applied to your live stream.',
};

const TONES: {[Kind in ScreenShareDeliveryNotice['kind']]: ScreenShareDeliveryNoticeTone} = {
	'device-framerate': 'info',
	'device-resolution': 'info',
	'device-crossover': 'info',
	'codec-switched': 'info',
	'device-short': 'warning',
	'connection-short': 'warning',
	'browser-resolution': 'warning',
	'capture-resolution': 'warning',
	'started-lower': 'info',
	'pinned-codec-short': 'warning',
	'encoder-recovered': 'info',
	'codec-viewer': 'info',
	'codec-device': 'info',
	'apply-failed': 'warning',
};

function unnamedPlaceholders(descriptor: MessageDescriptor): Array<string> {
	const comment = descriptor.comment ?? '';
	return [...(descriptor.message ?? '').matchAll(/\{(\w+)\}/g)]
		.map((match) => match[1])
		.filter((name) => !comment.includes(name));
}

const STATUS_DESCRIPTORS = [
	SCREEN_SHARE_STATUS_SENDING_DESCRIPTOR,
	SCREEN_SHARE_STATUS_ASKED_FOR_DESCRIPTOR,
	SCREEN_SHARE_STATUS_SOURCE_RATE_DESCRIPTOR,
	SCREEN_SHARE_STATUS_SOURCE_RESOLUTION_DESCRIPTOR,
	SCREEN_SHARE_TRY_FULL_QUALITY_AGAIN_DESCRIPTOR,
	SCREEN_SHARE_SETTINGS_ADJUSTED_DESCRIPTOR,
];

describe('formatScreenShareDeliveryNotice', () => {
	it.each(Object.values(NOTICES).map((notice) => [notice.kind, notice] as const))(
		'formats the %s notice through the i18n catalogue',
		(kind, notice) => {
			expect(formatScreenShareDeliveryNotice(pseudoI18n, notice)).toBe(`⟦${EXPECTED[kind]}⟧`);
		},
	);

	it('covers every notice kind', () => {
		expect(Object.keys(NOTICES).sort()).toEqual(Object.keys(EXPECTED).sort());
		expect(Object.keys(TEMPLATES).sort()).toEqual(Object.keys(EXPECTED).sort());
	});

	it.each(Object.values(NOTICES).map((notice) => [notice.kind, notice] as const))(
		'hands the %s template and its values to the catalogue uninterpolated',
		(kind, notice) => {
			const {descriptor, values} = captureNoticeDescriptor(notice);
			expect(descriptor.message).toBe(TEMPLATES[kind]);
			expect(Object.keys(values ?? {}).sort()).toEqual(
				[...TEMPLATES[kind].matchAll(/\{(\w+)\}/g)].map((match) => match[1]).sort(),
			);
			expect(descriptor.comment).toEqual(expect.stringMatching(/\S/));
			expect(unnamedPlaceholders(descriptor)).toEqual([]);
		},
	);

	it.each([
		[
			'device-short',
			8.2,
			'Your device cannot keep up. Your stream is set to 15 FPS at 2714×762 and viewers are getting about 8 FPS.',
		],
		[
			'device-short',
			8.6,
			'Your device cannot keep up. Your stream is set to 15 FPS at 2714×762 and viewers are getting about 9 FPS.',
		],
		[
			'connection-short',
			8.2,
			'Your connection cannot keep up. Your stream is set to 30 FPS at 720p and viewers are getting about 8 FPS.',
		],
		[
			'connection-short',
			8.6,
			'Your connection cannot keep up. Your stream is set to 30 FPS at 720p and viewers are getting about 9 FPS.',
		],
	] as const)('rounds the %s delivered rate %d to the nearest whole number', (kind, deliveredFrameRate, expected) => {
		const notice = {...NOTICES[kind], deliveredFrameRate};
		expect(formatScreenShareDeliveryNotice(pseudoI18n, notice)).toBe(`⟦${expected}⟧`);
	});
});

describe('screen share status and action descriptors', () => {
	it.each([
		[
			SCREEN_SHARE_STATUS_SENDING_DESCRIPTOR,
			'Set to send {resolution} at {frameRate} FPS with {codec}',
			{resolution: '1080p', frameRate: 60, codec: 'AV1'},
			'Set to send 1080p at 60 FPS with AV1',
		],
		[
			SCREEN_SHARE_STATUS_ASKED_FOR_DESCRIPTOR,
			'You asked for {resolution} at {frameRate} FPS',
			{resolution: '1440p', frameRate: 30},
			'You asked for 1440p at 30 FPS',
		],
		[
			SCREEN_SHARE_STATUS_SOURCE_RATE_DESCRIPTOR,
			'Your source is producing about {sourceFrameRate} FPS',
			{sourceFrameRate: 12},
			'Your source is producing about 12 FPS',
		],
		[SCREEN_SHARE_STATUS_SOURCE_RESOLUTION_DESCRIPTOR, 'Source', undefined, 'Source'],
		[SCREEN_SHARE_TRY_FULL_QUALITY_AGAIN_DESCRIPTOR, 'Try full quality again', undefined, 'Try full quality again'],
		[
			SCREEN_SHARE_SETTINGS_ADJUSTED_DESCRIPTOR,
			'Stream settings were adjusted',
			undefined,
			'Stream settings were adjusted',
		],
	])('renders %j as the design wording', (descriptor, template, values, expected) => {
		expect(descriptor.message).toBe(template);
		expect(descriptor.comment).toEqual(expect.stringMatching(/\S/));
		expect(unnamedPlaceholders(descriptor)).toEqual([]);
		expect(pseudoI18n._(descriptor, values)).toBe(`⟦${expected}⟧`);
	});
});

describe('resolveScreenShareDeliveryNoticeTone', () => {
	it('splits every notice kind into the tones the design asks for', () => {
		const byTone = (wanted: ScreenShareDeliveryNoticeTone) =>
			Object.values(NOTICES)
				.filter((notice) => resolveScreenShareDeliveryNoticeTone(notice) === wanted)
				.map((notice) => notice.kind)
				.sort();
		expect(byTone('warning')).toEqual([
			'apply-failed',
			'browser-resolution',
			'capture-resolution',
			'connection-short',
			'device-short',
			'pinned-codec-short',
		]);
		expect(byTone('info')).toEqual([
			'codec-device',
			'codec-switched',
			'codec-viewer',
			'device-crossover',
			'device-framerate',
			'device-resolution',
			'encoder-recovered',
			'started-lower',
		]);
	});
});

describe('formatScreenShareResolutionLabel', () => {
	it.each([
		[1920, 1080, '1080p'],
		[1280, 720, '720p'],
		[852, 480, '480p'],
		[854, 480, '480p'],
		[2560, 1440, '1440p'],
		[3840, 2160, '2160p'],
		[2714, 762, '2714×762'],
		[640, 480, '640×480'],
		[1080, 1920, '1080×1920'],
		[800, 600, '800×600'],
		[1600, 900, '1600×900'],
		[1280, 800, '1280×800'],
		[860, 480, '480p'],
		[864, 480, '864×480'],
		[1910, 1080, '1080p'],
		[1940, 1080, '1940×1080'],
		[1900, 1080, '1900×1080'],
	])('labels %ix%i as %s', (width, height, expected) => {
		expect(formatScreenShareResolutionLabel(width, height)).toBe(expected);
	});
});

describe('screen share message punctuation', () => {
	it('keeps every new message free of semicolons, em dashes and colons', () => {
		const descriptors = [
			...Object.values(NOTICES).map((notice) => captureNoticeDescriptor(notice).descriptor),
			...STATUS_DESCRIPTORS,
		];
		expect(descriptors).toHaveLength(20);
		for (const descriptor of descriptors) {
			expect(descriptor.message).toEqual(expect.any(String));
			expect(descriptor.message).not.toMatch(/[;:—–]/);
		}
	});
});

const plainI18n = fakeI18n(renderIcu);

const STATUS_TARGET: ScreenShareTarget = {
	mode: 'screenshare',
	resolution: 'ultra',
	frameRate: 30,
	width: 2560,
	height: 1440,
	rung: 'ultra',
	maxBitrate: 4_000_000,
	contentHint: 'text',
	keep: 'resolution',
	presetOwned: true,
	tierLimited: false,
	deviceMapped: false,
	memoryKey: 'ultra@ultra|30|screen',
};

function buildPlan(overrides: Partial<ScreenShareDeliveryPlan> = {}): ScreenShareDeliveryPlan {
	return {
		target: STATUS_TARGET,
		codec: 'vp9',
		level: {index: 0, segment: 0, frameRate: 30, width: 2560, height: 1440, rung: 'ultra', maxBitrate: 4_000_000},
		levelIndex: 0,
		deliveredFrameRate: 29,
		sentWidth: 2560,
		sentHeight: 1440,
		sourceFrameRate: null,
		holdingShort: false,
		...overrides,
	};
}

const SOURCE_BOX_TARGET: ScreenShareTarget = {
	...STATUS_TARGET,
	resolution: 'source',
	frameRate: 15,
	width: 3840,
	height: 2160,
	rung: 'source',
	maxBitrate: 4_500_000,
	memoryKey: 'source@source|15|screen',
};

const SOURCE_FITTED_TARGET: ScreenShareTarget = {
	...SOURCE_BOX_TARGET,
	width: 2560,
	height: 1440,
	rung: 'ultra',
	memoryKey: 'source@ultra|15|screen',
};

const STEPPED_PLAN = buildPlan({
	levelIndex: 2,
	level: {index: 2, segment: 0, frameRate: 15, width: 2560, height: 1440, rung: 'ultra', maxBitrate: 3_000_000},
	deliveredFrameRate: 13.8,
});

describe('formatScreenShareDeliveryStatus', () => {
	it('reports the level the share is set to send while it runs at the asked-for level', () => {
		expect(formatScreenShareDeliveryStatus(plainI18n, buildPlan(), null)).toEqual({
			sending: 'Set to send 1440p at 30 FPS with VP9',
			askedFor: null,
			notice: null,
			sourceRate: null,
			hasRows: true,
			canTryFullQuality: false,
		});
	});

	it('adds what was asked for and the action once the share has stepped down', () => {
		expect(formatScreenShareDeliveryStatus(plainI18n, STEPPED_PLAN, NOTICES['device-framerate'])).toEqual({
			sending: 'Set to send 1440p at 15 FPS with VP9',
			askedFor: 'You asked for 1440p at 30 FPS',
			notice: 'Your device could not keep up, so your stream now sends 15 FPS to keep 1080p sharp.',
			sourceRate: null,
			hasRows: true,
			canTryFullQuality: true,
		});
	});

	it('leaves out what was asked for while the share holds short at the level it asked for', () => {
		expect(
			formatScreenShareDeliveryStatus(plainI18n, buildPlan({holdingShort: true}), NOTICES['device-short']),
		).toEqual({
			sending: 'Set to send 1440p at 30 FPS with VP9',
			askedFor: null,
			notice:
				'Your device cannot keep up. Your stream is set to 15 FPS at 2714×762 and viewers are getting about 9 FPS.',
			sourceRate: null,
			hasRows: true,
			canTryFullQuality: false,
		});
	});

	it('adds what was asked for once the level drops only the frame rate below it', () => {
		const plan = buildPlan({
			level: {index: 1, segment: 0, frameRate: 15, width: 2560, height: 1440, rung: 'ultra', maxBitrate: 3_000_000},
			levelIndex: 1,
			holdingShort: true,
		});
		expect(formatScreenShareDeliveryStatus(plainI18n, plan, null).askedFor).toBe('You asked for 1440p at 30 FPS');
	});

	it('shows a start notice and the action before the first sample lands', () => {
		expect(formatScreenShareDeliveryStatus(plainI18n, null, NOTICES['started-lower'])).toEqual({
			sending: null,
			askedFor: null,
			notice: 'This device could not keep up with 1440p at 60 FPS last time, so your stream started at 720p at 30 FPS.',
			sourceRate: null,
			hasRows: true,
			canTryFullQuality: true,
		});
	});

	it('states the level box rather than the size and rate the sample measured', () => {
		const plan = buildPlan({sentWidth: 1920, sentHeight: 1080, deliveredFrameRate: 7.4});
		expect(formatScreenShareDeliveryStatus(plainI18n, plan, null).sending).toBe('Set to send 1440p at 30 FPS with VP9');
	});

	it('takes the sending row from the level the share settled on and not the box it asked for', () => {
		const plan = buildPlan({
			levelIndex: 3,
			level: {index: 3, segment: 0, frameRate: 15, width: 1280, height: 720, rung: 'medium', maxBitrate: 1_500_000},
			sentWidth: 1920,
			sentHeight: 1080,
			deliveredFrameRate: 7.4,
		});
		const status = formatScreenShareDeliveryStatus(plainI18n, plan, null);
		expect(status.sending).toBe('Set to send 720p at 15 FPS with VP9');
		expect(status.askedFor).toBe('You asked for 1440p at 30 FPS');
	});

	it('leaves the rate reaching viewers to the notice while the connection holds the share short', () => {
		const plan = buildPlan({
			level: {index: 3, segment: 1, frameRate: 15, width: 1280, height: 720, rung: 'medium', maxBitrate: 1_500_000},
			levelIndex: 3,
			codec: 'h264',
			sentWidth: 1280,
			sentHeight: 720,
			deliveredFrameRate: 7.5,
			holdingShort: true,
		});
		const status = formatScreenShareDeliveryStatus(plainI18n, plan, {
			kind: 'connection-short',
			width: 1280,
			height: 720,
			frameRate: 15,
			deliveredFrameRate: 7.4,
		});
		expect(status.sending).toBe('Set to send 720p at 15 FPS with H.264');
		expect(status.notice).toBe(
			'Your connection cannot keep up. Your stream is set to 15 FPS at 720p and viewers are getting about 7 FPS.',
		);
		expect(status.askedFor).toBe('You asked for 1440p at 30 FPS');
	});

	it('has a sending row before the first sample reports anything measured', () => {
		const plan = buildPlan({sentWidth: null, sentHeight: null, deliveredFrameRate: null});
		expect(formatScreenShareDeliveryStatus(plainI18n, plan, null).sending).toBe('Set to send 1440p at 30 FPS with VP9');
	});

	it('calls an asked-for source target without source dimensions Source', () => {
		const plan = buildPlan({
			target: SOURCE_BOX_TARGET,
			levelIndex: 2,
			level: {index: 2, segment: 1, frameRate: 15, width: 2560, height: 1440, rung: 'ultra', maxBitrate: 4_000_000},
		});
		expect(formatScreenShareDeliveryStatus(plainI18n, plan, null).askedFor).toBe('You asked for Source at 15 FPS');
	});

	it('names the fitted size once an asked-for source target has source dimensions', () => {
		const plan = buildPlan({
			target: SOURCE_FITTED_TARGET,
			levelIndex: 2,
			level: {index: 2, segment: 1, frameRate: 15, width: 1920, height: 1080, rung: 'high', maxBitrate: 3_000_000},
		});
		expect(formatScreenShareDeliveryStatus(plainI18n, plan, null).askedFor).toBe('You asked for 1440p at 15 FPS');
	});

	it('has no rows to show before a plan or a notice exists', () => {
		expect(formatScreenShareDeliveryStatus(plainI18n, null, null)).toEqual({
			sending: null,
			askedFor: null,
			notice: null,
			sourceRate: null,
			hasRows: false,
			canTryFullQuality: false,
		});
	});

	it('rounds the source rate it reports', () => {
		expect(formatScreenShareDeliveryStatus(plainI18n, buildPlan({sourceFrameRate: 23.6}), null).sourceRate).toBe(
			'Your source is producing about 24 FPS',
		);
	});

	it.each([
		['device-framerate', true],
		['device-resolution', true],
		['device-crossover', true],
		['device-short', true],
		['started-lower', true],
		['apply-failed', true],
		['connection-short', false],
		['browser-resolution', false],
		['capture-resolution', false],
		['codec-switched', false],
		['codec-viewer', false],
		['codec-device', false],
		['pinned-codec-short', false],
		['encoder-recovered', false],
	] as const)('offers the full quality action next to the %s notice: %s', (kind, expected) => {
		expect(formatScreenShareDeliveryStatus(plainI18n, STEPPED_PLAN, NOTICES[kind]).canTryFullQuality).toBe(expected);
	});

	it('covers every notice kind in the action table', () => {
		expect(Object.keys(NOTICES)).toHaveLength(14);
	});
});

const LIVE_SHARE_SIGNALS = {
	isConnected: true,
	canStream: true,
	isCameraEnabled: false,
	isCameraUserCapReached: false,
	isScreenShareEnabled: true,
};

describe('selectLocalParticipantControlsViewState screen share notices', () => {
	it('keeps the existing label with no notice', () => {
		expect(
			selectLocalParticipantControlsViewState({...LIVE_SHARE_SIGNALS, screenShareNoticeTone: null}).screenShare,
		).toMatchObject({labelKey: 'configureOrEndScreenShare', showsNotice: false, showsNoticeDot: false});
	});

	it('shows a warning notice with a dot', () => {
		expect(
			selectLocalParticipantControlsViewState({...LIVE_SHARE_SIGNALS, screenShareNoticeTone: 'warning'}).screenShare,
		).toMatchObject({labelKey: 'configureOrEndScreenShare', showsNotice: true, showsNoticeDot: true});
	});

	it('shows an info notice without a dot', () => {
		expect(
			selectLocalParticipantControlsViewState({...LIVE_SHARE_SIGNALS, screenShareNoticeTone: 'info'}).screenShare,
		).toMatchObject({showsNotice: true, showsNoticeDot: false});
	});

	it('drops a leftover notice once the share is no longer live', () => {
		expect(
			selectLocalParticipantControlsViewState({
				...LIVE_SHARE_SIGNALS,
				isScreenShareEnabled: false,
				screenShareNoticeTone: 'warning',
			}).screenShare,
		).toMatchObject({labelKey: 'shareScreen', showsNotice: false, showsNoticeDot: false});
	});

	it('takes the dot from the notice tone the tile badge uses', () => {
		for (const notice of Object.values(NOTICES)) {
			const state = selectLocalParticipantControlsViewState({
				...LIVE_SHARE_SIGNALS,
				screenShareNoticeTone: resolveScreenShareDeliveryNoticeTone(notice),
			}).screenShare;
			expect(state.showsNotice).toBe(true);
			expect(state.showsNoticeDot).toBe(TONES[notice.kind] === 'warning');
		}
	});
});
