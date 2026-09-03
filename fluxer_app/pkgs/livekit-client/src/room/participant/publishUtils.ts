// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import log from '../../logger.ts';
import {getBrowser} from '../../utils/browserParser.ts';
import {TrackInvalidError} from '../errors.ts';
import LocalAudioTrack from '../track/LocalAudioTrack.ts';
import LocalVideoTrack from '../track/LocalVideoTrack.ts';
import type {BackupVideoCodec, TrackPublishOptions, VideoCodec, VideoEncoding} from '../track/options.ts';
import {ScreenSharePresets, VideoPreset, VideoPresets, VideoPresets43} from '../track/options.ts';
import {Track} from '../track/Track.ts';
import type {LoggerOptions} from '../types.ts';
import {
	compareVersions,
	getReactNativeOs,
	isFireFox,
	isReactNative,
	isSafariBased,
	isSafariSvcApi,
	isSVCCodec,
	unwrapConstraint,
} from '../utils.ts';

export function mediaTrackToLocalTrack(
	mediaStreamTrack: MediaStreamTrack,
	constraints?: MediaTrackConstraints,
	loggerOptions?: LoggerOptions,
): LocalVideoTrack | LocalAudioTrack {
	switch (mediaStreamTrack.kind) {
		case 'audio':
			return new LocalAudioTrack(mediaStreamTrack, constraints, false, undefined, loggerOptions);
		case 'video':
			return new LocalVideoTrack(mediaStreamTrack, constraints, false, loggerOptions);
		default:
			throw new TrackInvalidError(`unsupported track type: ${mediaStreamTrack.kind}`);
	}
}

export const presets169 = Object.values(VideoPresets);

export const presets43 = Object.values(VideoPresets43);

export const presetsScreenShare = Object.values(ScreenSharePresets);

export const defaultSimulcastPresets169 = [VideoPresets.h180, VideoPresets.h360];

export const defaultSimulcastPresets43 = [VideoPresets43.h180, VideoPresets43.h360];

export const computeDefaultScreenShareSimulcastPresets = (fromPreset: VideoPreset) => {
	const layers = [{scaleResolutionDownBy: 2, fps: fromPreset.encoding.maxFramerate}];
	return layers.map(
		(t) =>
			new VideoPreset(
				Math.floor(fromPreset.width / t.scaleResolutionDownBy),
				Math.floor(fromPreset.height / t.scaleResolutionDownBy),
				Math.max(
					150_000,
					Math.floor(
						fromPreset.encoding.maxBitrate /
							(t.scaleResolutionDownBy ** 2 * ((fromPreset.encoding.maxFramerate ?? 30) / (t.fps ?? 30))),
					),
				),
				t.fps,
				fromPreset.encoding.priority,
			),
	);
};

const videoRids = ['q', 'h', 'f'];

export function computeVideoEncodings(
	isScreenShare: boolean,
	width?: number,
	height?: number,
	options?: TrackPublishOptions,
): Array<RTCRtpEncodingParameters> {
	let videoEncoding: VideoEncoding | undefined = options?.videoEncoding;

	if (isScreenShare) {
		videoEncoding = options?.screenShareEncoding;
	}

	const useSimulcast = options?.simulcast;
	const scalabilityMode = options?.scalabilityMode;
	const videoCodec = options?.videoCodec;

	if ((!videoEncoding && !useSimulcast && !scalabilityMode) || !width || !height) {
		return [{}];
	}

	if (!videoEncoding) {
		videoEncoding = determineAppropriateEncoding(isScreenShare, width, height, videoCodec);
		log.debug('using video encoding', videoEncoding);
	}

	const sourceFramerate = videoEncoding.maxFramerate;

	const original = new VideoPreset(
		width,
		height,
		videoEncoding.maxBitrate,
		videoEncoding.maxFramerate,
		videoEncoding.priority,
	);

	if (scalabilityMode && isSVCCodec(videoCodec)) {
		const sm = new ScalabilityMode(scalabilityMode);

		const encodings: Array<RTCRtpEncodingParameters> = [];

		if (sm.spatial > 3) {
			throw new Error(`unsupported scalabilityMode: ${scalabilityMode}`);
		}
		const browser = getBrowser();
		if (
			isSafariBased() ||
			isReactNative() ||
			(browser?.name === 'Chrome' && compareVersions(browser?.version, '113') < 0)
		) {
			const bitratesRatio = sm.suffix === 'h' ? 2 : 3;
			const requireScale = isSafariSvcApi(browser);
			for (let i = 0; i < sm.spatial; i += 1) {
				encodings.push({
					rid: videoRids[2 - i],
					maxBitrate: videoEncoding.maxBitrate / bitratesRatio ** i,
					maxFramerate: original.encoding.maxFramerate,
					scaleResolutionDownBy: requireScale ? 2 ** i : undefined,
				});
			}
			encodings[0].scalabilityMode = scalabilityMode;
		} else {
			encodings.push({
				maxBitrate: videoEncoding.maxBitrate,
				maxFramerate: original.encoding.maxFramerate,
				scalabilityMode: scalabilityMode,
			});
		}

		if (original.encoding.priority) {
			encodings[0].priority = original.encoding.priority;
			encodings[0].networkPriority = original.encoding.priority;
		}

		log.debug(`using svc encoding`, {encodings});
		return encodings;
	}

	if (!useSimulcast) {
		return [videoEncoding];
	}

	let presets: Array<VideoPreset> = [];
	if (isScreenShare) {
		presets = sortPresets(options?.screenShareSimulcastLayers) ?? defaultSimulcastLayers(isScreenShare, original);
	} else {
		presets = sortPresets(options?.videoSimulcastLayers) ?? defaultSimulcastLayers(isScreenShare, original);
	}
	let midPreset: VideoPreset | undefined;
	if (presets.length > 0) {
		const lowPreset = presets[0];
		if (presets.length > 1) {
			[, midPreset] = presets;
		}

		const size = Math.max(width, height);
		if (size >= 960 && midPreset) {
			return encodingsFromPresets(width, height, [lowPreset, midPreset, original], sourceFramerate);
		}
		if (size >= 480) {
			return encodingsFromPresets(width, height, [lowPreset, original], sourceFramerate);
		}
	}
	return encodingsFromPresets(width, height, [original]);
}

export function computeTrackBackupEncodings(
	track: LocalVideoTrack,
	videoCodec: BackupVideoCodec,
	opts: TrackPublishOptions,
) {
	if (!opts.backupCodec || opts.backupCodec === true || opts.backupCodec.codec === opts.videoCodec) {
		return;
	}
	if (videoCodec !== opts.backupCodec.codec) {
		log.warn('requested a different codec than specified as backup', {
			serverRequested: videoCodec,
			backup: opts.backupCodec.codec,
		});
	}

	opts.videoCodec = videoCodec;
	opts.videoEncoding = opts.backupCodec.encoding;

	const settings = track.mediaStreamTrack.getSettings();
	const width = settings.width ?? track.dimensions?.width;
	const height = settings.height ?? track.dimensions?.height;

	if (track.source === Track.Source.ScreenShare && opts.simulcast) {
		opts.simulcast = false;
	}
	const encodings = computeVideoEncodings(track.source === Track.Source.ScreenShare, width, height, opts);
	return encodings;
}

export function determineAppropriateEncoding(
	isScreenShare: boolean,
	width: number,
	height: number,
	codec?: VideoCodec,
): VideoEncoding {
	const presets = presetsForResolution(isScreenShare, width, height);
	let {encoding} = presets[0];

	const size = Math.max(width, height);

	for (let i = 0; i < presets.length; i += 1) {
		const preset = presets[i];
		encoding = preset.encoding;
		if (preset.width >= size) {
			break;
		}
	}

	if (codec) {
		switch (codec) {
			case 'av1':
			case 'h265':
				encoding = {...encoding};
				encoding.maxBitrate = encoding.maxBitrate * 0.7;
				break;
			case 'vp9':
				encoding = {...encoding};
				encoding.maxBitrate = encoding.maxBitrate * 0.85;
				break;
			default:
				break;
		}
	}

	return encoding;
}

export function presetsForResolution(isScreenShare: boolean, width: number, height: number): Array<VideoPreset> {
	if (isScreenShare) {
		return presetsScreenShare;
	}
	const aspect = width > height ? width / height : height / width;
	if (Math.abs(aspect - 16.0 / 9) < Math.abs(aspect - 4.0 / 3)) {
		return presets169;
	}
	return presets43;
}

export function defaultSimulcastLayers(isScreenShare: boolean, original: VideoPreset): Array<VideoPreset> {
	if (isScreenShare) {
		return computeDefaultScreenShareSimulcastPresets(original);
	}
	const {width, height} = original;
	const aspect = width > height ? width / height : height / width;
	if (Math.abs(aspect - 16.0 / 9) < Math.abs(aspect - 4.0 / 3)) {
		return defaultSimulcastPresets169;
	}
	return defaultSimulcastPresets43;
}

function encodingsFromPresets(
	width: number,
	height: number,
	presets: Array<VideoPreset>,
	sourceFramerate?: number | undefined,
): Array<RTCRtpEncodingParameters> {
	const encodings: Array<RTCRtpEncodingParameters> = [];
	presets.forEach((preset, idx) => {
		if (idx >= videoRids.length) {
			return;
		}
		const size = Math.min(width, height);
		const rid = videoRids[idx];

		const encoding: RTCRtpEncodingParameters = {
			rid,
			scaleResolutionDownBy: Math.max(1, size / Math.min(preset.width, preset.height)),
			maxBitrate: preset.encoding.maxBitrate,
		};
		const maxFramerate =
			sourceFramerate && preset.encoding.maxFramerate
				? Math.min(sourceFramerate, preset.encoding.maxFramerate)
				: preset.encoding.maxFramerate;
		if (maxFramerate) {
			encoding.maxFramerate = maxFramerate;
		}
		const canSetPriority = isFireFox() || idx === 0;
		if (preset.encoding.priority && canSetPriority) {
			encoding.priority = preset.encoding.priority;
			encoding.networkPriority = preset.encoding.priority;
		}
		encodings.push(encoding);
	});

	if (isReactNative() && getReactNativeOs() === 'ios') {
		let topFramerate: number | undefined;
		encodings.forEach((encoding) => {
			if (!topFramerate) {
				topFramerate = encoding.maxFramerate;
			} else if (encoding.maxFramerate && encoding.maxFramerate > topFramerate) {
				topFramerate = encoding.maxFramerate;
			}
		});

		let notifyOnce = true;
		encodings.forEach((encoding) => {
			if (encoding.maxFramerate !== topFramerate) {
				if (notifyOnce) {
					notifyOnce = false;
					log.info(`Simulcast on iOS React-Native requires all encodings to share the same framerate.`);
				}
				log.info(`Setting framerate of encoding "${encoding.rid ?? ''}" to ${topFramerate}`);
				encoding.maxFramerate = topFramerate;
			}
		});
	}

	return encodings;
}

export function sortPresets(presets: Array<VideoPreset> | undefined) {
	if (!presets) return;
	return presets.sort((a, b) => {
		const {encoding: aEnc} = a;
		const {encoding: bEnc} = b;

		if (aEnc.maxBitrate > bEnc.maxBitrate) {
			return 1;
		}
		if (aEnc.maxBitrate < bEnc.maxBitrate) return -1;
		if (aEnc.maxBitrate === bEnc.maxBitrate && aEnc.maxFramerate && bEnc.maxFramerate) {
			return aEnc.maxFramerate > bEnc.maxFramerate ? 1 : -1;
		}
		return 0;
	});
}

export class ScalabilityMode {
	spatial: number;

	temporal: number;

	suffix: undefined | 'h' | '_KEY' | '_KEY_SHIFT';

	constructor(scalabilityMode: string) {
		const results = scalabilityMode.match(/^L(\d)T(\d)(h|_KEY|_KEY_SHIFT){0,1}$/);
		if (!results) {
			throw new Error('invalid scalability mode');
		}

		this.spatial = parseInt(results[1], 10);
		this.temporal = parseInt(results[2], 10);
		if (results.length > 3) {
			switch (results[3]) {
				case 'h':
				case '_KEY':
				case '_KEY_SHIFT':
					this.suffix = results[3];
			}
		}
	}

	toString(): string {
		return `L${this.spatial}T${this.temporal}${this.suffix ?? ''}`;
	}
}

export function maxEncodingBitrate(encodings: Array<RTCRtpEncodingParameters> | undefined): number {
	if (!encodings) {
		return 0;
	}
	let maxBitrate = 0;
	for (const encoding of encodings) {
		if (typeof encoding.maxBitrate === 'number' && encoding.maxBitrate > maxBitrate) {
			maxBitrate = encoding.maxBitrate;
		}
	}
	return maxBitrate;
}

export function getDefaultDegradationPreference(track: LocalVideoTrack): RTCDegradationPreference {
	if (track.source === Track.Source.ScreenShare) return 'maintain-resolution';
	if (track.constraints.height && unwrapConstraint(track.constraints.height) >= 1080) return 'maintain-resolution';
	return 'balanced';
}
