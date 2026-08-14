// SPDX-License-Identifier: AGPL-3.0-or-later

import {LimitResolver} from '@app/features/app/utils/LimitResolverAdapter';
import {isLimitToggleEnabled} from '@app/features/app/utils/LimitUtils';
import AppStorage from '@app/features/platform/state/PersistentStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import type {
	CodecPreference,
	ScreenShareBackupCodecMode,
	ScreenShareContentHint,
	ScreenShareEncoderMode,
	ScreenShareScalabilityModePreference,
	ScreenShareSoftwareQuality,
} from '@app/features/voice/utils/CodecCapabilityDetector';
import {areVoiceBackgroundsAvailable} from '@app/features/voice/utils/VoiceBackgroundAvailability';
import {
	DEFAULT_VOICE_PROCESSING_MODE,
	type VoiceProcessingMode,
} from '@app/features/voice/utils/VoiceProcessingProfile';
import {clampVoiceVolumePercent} from '@app/features/voice/utils/VoiceVolumeUtils';
import type {UserPrivate} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {makeAutoObservable} from 'mobx';

export type VoiceBackgroundMediaKind = 'static' | 'animated' | 'video';

export interface BackgroundImage {
	id: string;
	createdAt: number;
	mediaKind?: VoiceBackgroundMediaKind;
}

export const NONE_BACKGROUND_ID = 'none';
export const BLUR_BACKGROUND_ID = 'blur';

export type CameraResolution = 'low' | 'medium' | 'high';
export type ScreenshareResolution = 'low_240p' | 'low_480p' | 'medium' | 'high' | 'ultra' | 'source';
export type StreamingMode = 'gaming' | 'screenshare' | 'custom';
export type LastScreenShareSourceKind = 'app' | 'display' | 'device' | 'game';

export interface LastScreenShareSource {
	kind: LastScreenShareSourceKind;
	sourceId: string | null;
	title: string;
	updatedAt: number;
}

const logger = new Logger('VoiceSettings');

const MAX_VOICE_PROCESSING_DEVICE_OVERRIDES = 16;
const VIDEO_FRAME_RATE_MIN = 15;
const VIDEO_FRAME_RATE_MAX = 120;
const VIDEO_FRAME_RATE_DEFAULT = 30;
export const CAMERA_EFFECT_STRENGTH_MIN = 0;
export const CAMERA_EFFECT_STRENGTH_MAX = 100;
export const CAMERA_EFFECT_STRENGTH_DEFAULT = 50;
export const DEFAULT_SCREEN_SHARE_CONTENT_HINT: ScreenShareContentHint = 'auto';
export const DEFAULT_SCREEN_SHARE_ENCODER_MODE: ScreenShareEncoderMode = 'auto';
export const DEFAULT_SCREEN_SHARE_SOFTWARE_QUALITY: ScreenShareSoftwareQuality = 'balanced';
export const DEFAULT_SCREEN_SHARE_SCALABILITY_MODE: ScreenShareScalabilityModePreference = 'auto';
export const DEFAULT_SCREEN_SHARE_BACKUP_CODEC_MODE: ScreenShareBackupCodecMode = 'off';
export const DEFAULT_SCREEN_SHARE_MAX_BITRATE_MBPS = 50;

type VoiceSettingsUpdate = Partial<{
	inputDeviceId: string;
	outputDeviceId: string;
	videoDeviceId: string;
	inputVolume: number;
	outputVolume: number;
	echoCancellation: boolean;
	noiseSuppression: boolean;
	autoGainControl: boolean;
	deepFilterNoiseSuppression: boolean;
	deepFilterNoiseSuppressionLevel: number;
	voiceProcessingMode: VoiceProcessingMode;
	cameraResolution: CameraResolution;
	mirrorCamera: boolean;
	screenshareResolution: ScreenshareResolution;
	videoFrameRate: number;
	streamingMode: StreamingMode;
	hideStreamPreview: boolean;
	muteStreamAudio: boolean;
	shareAppAudio: boolean;
	shareDesktopAudio: boolean;
	shareDeviceAudio: boolean;
	screenShareAudioDeviceId: string;
	backgroundImageId: string;
	backgroundImages: Array<BackgroundImage>;
	backgroundBlurStrength: number;
	showGridView: boolean;
	showMyOwnCamera: boolean;
	showMyOwnScreenShare: boolean;
	showNonVideoParticipants: boolean;
	showParticipantsCarousel: boolean;
	showVoiceConnectionAvatarStack: boolean;
	showVoiceConnectionId: boolean;
	showConnectionVolumeControls: boolean;
	pauseOwnScreenSharePreviewOnUnfocus: boolean;
	disablePictureInPicturePopoutScreenShare: boolean;
	preferredVideoCodec: CodecPreference;
	preferredScreenShareCodec: CodecPreference;
	emulatedDecodeVideoCodecCap: CodecPreference;
	screenShareContentHint: ScreenShareContentHint;
	screenShareEncoderMode: ScreenShareEncoderMode;
	screenShareSoftwareQuality: ScreenShareSoftwareQuality;
	screenShareScalabilityMode: ScreenShareScalabilityModePreference;
	screenShareBackupCodecMode: ScreenShareBackupCodecMode;
	screenShareMaxBitrateMbps: number;
	adaptiveScreenShareQuality: boolean;
	vadThreshold: number;
	vadAutoSensitivity: boolean;
	vadEnhanced: boolean;
	linuxAudioCaptureWorkaround: boolean;
	linuxAudioCaptureOnlySpeakers: boolean;
	linuxAudioCaptureOnlyDefaultSpeakers: boolean;
	linuxAudioCaptureIgnoreInputMedia: boolean;
	linuxAudioCaptureIgnoreVirtual: boolean;
	linuxAudioCaptureIgnoreDevices: boolean;
	linuxAudioCaptureGranularSelect: boolean;
	linuxAudioCaptureDeviceSelect: boolean;
	screenShareAudioSourceMode: 'none' | 'system' | 'specific';
	screenShareAudioIncludeSources: Array<Record<string, string>>;
	screenShareAudioExcludeSources: Array<Record<string, string>>;
	openH264Enabled: boolean;
	lastScreenShareSource: LastScreenShareSource | null;
}>;

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function parseVoiceSettingsStorage(raw: string | null): Record<string, unknown> {
	if (!raw) {
		return {};
	}
	const parsed: unknown = JSON.parse(raw);
	return isRecord(parsed) ? parsed : {};
}

function isVoiceBackgroundMediaKind(value: unknown): value is VoiceBackgroundMediaKind {
	return value === 'static' || value === 'animated' || value === 'video';
}

function clampVideoFrameRate(value: unknown): number {
	if (typeof value !== 'number' || !Number.isFinite(value)) {
		return VIDEO_FRAME_RATE_DEFAULT;
	}
	return Math.max(VIDEO_FRAME_RATE_MIN, Math.min(VIDEO_FRAME_RATE_MAX, value));
}

function clampCameraEffectStrength(value: unknown): number {
	if (typeof value !== 'number' || !Number.isFinite(value)) {
		return CAMERA_EFFECT_STRENGTH_DEFAULT;
	}
	return Math.max(CAMERA_EFFECT_STRENGTH_MIN, Math.min(CAMERA_EFFECT_STRENGTH_MAX, Math.round(value)));
}

function applyLegacyVenmicKeyMigration(parsed: Record<string, unknown>): boolean {
	const renames: Array<[string, string]> = [
		['venmicWorkaround', 'linuxAudioCaptureWorkaround'],
		['venmicOnlySpeakers', 'linuxAudioCaptureOnlySpeakers'],
		['venmicOnlyDefaultSpeakers', 'linuxAudioCaptureOnlyDefaultSpeakers'],
		['venmicIgnoreInputMedia', 'linuxAudioCaptureIgnoreInputMedia'],
		['venmicIgnoreVirtual', 'linuxAudioCaptureIgnoreVirtual'],
		['venmicIgnoreDevices', 'linuxAudioCaptureIgnoreDevices'],
		['venmicGranularSelect', 'linuxAudioCaptureGranularSelect'],
		['venmicDeviceSelect', 'linuxAudioCaptureDeviceSelect'],
	];
	let changed = false;
	for (const [from, to] of renames) {
		if (Object.hasOwn(parsed, from)) {
			if (!Object.hasOwn(parsed, to)) {
				parsed[to] = parsed[from];
			}
			delete parsed[from];
			changed = true;
		}
	}
	return changed;
}

function applyPiPPopoutDefaultsMigration(parsed: Record<string, unknown>): boolean {
	if (parsed.pipPopoutDefaultsMigratedV3 === true) {
		return false;
	}
	delete parsed.disablePictureInPicturePopout;
	if (typeof parsed.disablePictureInPicturePopoutScreenShare !== 'boolean') {
		parsed.disablePictureInPicturePopoutScreenShare = false;
	}
	parsed.pipPopoutDefaultsMigratedV3 = true;
	return true;
}

function applyAdaptiveScreenShareQualityMigrationV2(parsed: Record<string, unknown>): boolean {
	let changed = false;
	if (Object.hasOwn(parsed, 'adaptiveScreenShareQuality')) {
		delete parsed.adaptiveScreenShareQuality;
		changed = true;
	}
	if (typeof parsed.adaptiveScreenShareQualityPrefV2 !== 'boolean') {
		parsed.adaptiveScreenShareQualityPrefV2 = false;
		changed = true;
	}
	return changed;
}

function applyScreenShareAudioConsentMigrationV1(parsed: Record<string, unknown>): boolean {
	if (parsed.screenShareAudioConsentMigratedV1 === true) {
		return false;
	}
	parsed.shareAppAudio = false;
	parsed.shareDesktopAudio = false;
	parsed.shareDeviceAudio = false;
	parsed.muteStreamAudio = true;
	parsed.screenShareAudioConsentMigratedV1 = true;
	return true;
}

function applyStreamingModeDefaultMigrationV1(parsed: Record<string, unknown>): boolean {
	if (parsed.streamingModeDefaultMigratedV1 === true) {
		return false;
	}
	const resolution = parsed.screenshareResolution;
	const frameRate = parsed.videoFrameRate;
	const hasImplicitGamingDefault =
		parsed.streamingMode === undefined ||
		(parsed.streamingMode === 'gaming' &&
			(resolution === undefined || resolution === 'medium') &&
			(frameRate === undefined || frameRate === 30));
	if (hasImplicitGamingDefault) {
		parsed.streamingMode = 'screenshare';
	}
	parsed.streamingModeDefaultMigratedV1 = true;
	return true;
}

function validateBackgroundImages(images: unknown): Array<BackgroundImage> {
	if (!Array.isArray(images)) return [];
	const validated: Array<BackgroundImage> = [];
	for (const image of images) {
		if (!isRecord(image)) continue;
		if (typeof image.id !== 'string' || image.id.length === 0) continue;
		if (typeof image.createdAt !== 'number' || !Number.isFinite(image.createdAt)) continue;
		const next: BackgroundImage = {
			id: image.id,
			createdAt: image.createdAt,
		};
		if (isVoiceBackgroundMediaKind(image.mediaKind)) {
			next.mediaKind = image.mediaKind;
		}
		validated.push(next);
	}
	return validated;
}

class VoiceSettings {
	inputDeviceId = 'default';
	outputDeviceId = 'default';
	videoDeviceId = 'default';
	inputVolume = 100;
	outputVolume = 100;
	echoCancellation = true;
	noiseSuppression = true;
	autoGainControl = true;
	deepFilterNoiseSuppressionPrefV2 = true;
	deepFilterNoiseSuppressionLevelPrefV2 = 80;
	voiceProcessingMode: VoiceProcessingMode = DEFAULT_VOICE_PROCESSING_MODE;
	voiceProcessingModeByDeviceLabel: Record<string, VoiceProcessingMode> = {};
	cameraResolution: CameraResolution = 'medium';
	mirrorCamera = true;
	screenshareResolution: ScreenshareResolution = 'medium';
	videoFrameRate = 30;
	streamingMode: StreamingMode = 'screenshare';
	streamingModeDefaultMigratedV1 = false;
	hideStreamPreview = false;
	muteStreamAudio = true;
	shareAppAudio = false;
	shareDesktopAudio = false;
	shareDeviceAudio = false;
	screenShareAudioDeviceId = 'default';
	screenShareAudioConsentMigratedV1 = false;
	backgroundImageId = NONE_BACKGROUND_ID;
	backgroundImages: Array<BackgroundImage> = [];
	backgroundBlurStrength = CAMERA_EFFECT_STRENGTH_DEFAULT;
	showGridView = false;
	showMyOwnCamera = true;
	showMyOwnScreenShare = true;
	showNonVideoParticipants = true;
	showParticipantsCarousel = false;
	showVoiceConnectionAvatarStack = true;
	showVoiceConnectionIdPrefV2 = true;
	showConnectionVolumeControls = false;
	pauseOwnScreenSharePreviewOnUnfocusPrefV2 = true;
	disablePictureInPicturePopoutScreenShare = false;
	preferredVideoCodec: CodecPreference = 'auto';
	preferredScreenShareCodec: CodecPreference = 'auto';
	emulatedDecodeVideoCodecCap: CodecPreference = 'auto';
	screenShareContentHintPrefV2: ScreenShareContentHint = DEFAULT_SCREEN_SHARE_CONTENT_HINT;
	screenShareEncoderModePrefV2: ScreenShareEncoderMode = DEFAULT_SCREEN_SHARE_ENCODER_MODE;
	screenShareSoftwareQualityPrefV2: ScreenShareSoftwareQuality = DEFAULT_SCREEN_SHARE_SOFTWARE_QUALITY;
	screenShareScalabilityModePrefV2: ScreenShareScalabilityModePreference = DEFAULT_SCREEN_SHARE_SCALABILITY_MODE;
	screenShareBackupCodecModePrefV2: ScreenShareBackupCodecMode = DEFAULT_SCREEN_SHARE_BACKUP_CODEC_MODE;
	screenShareMaxBitrateMbpsPrefV2 = DEFAULT_SCREEN_SHARE_MAX_BITRATE_MBPS;
	adaptiveScreenShareQualityPrefV2 = false;
	vadThreshold = 50;
	vadAutoSensitivity = true;
	vadEnhanced = true;
	linuxAudioCaptureWorkaround = false;
	linuxAudioCaptureOnlySpeakers = true;
	linuxAudioCaptureOnlyDefaultSpeakers = true;
	linuxAudioCaptureIgnoreInputMedia = true;
	linuxAudioCaptureIgnoreVirtual = false;
	linuxAudioCaptureIgnoreDevices = true;
	linuxAudioCaptureGranularSelect = false;
	linuxAudioCaptureDeviceSelect = false;
	screenShareAudioSourceMode: 'none' | 'system' | 'specific' = 'system';
	screenShareAudioIncludeSources: Array<Record<string, string>> = [];
	screenShareAudioExcludeSources: Array<Record<string, string>> = [];
	openH264Enabled = true;
	lastScreenShareSource: LastScreenShareSource | null = null;
	prioritizeSpeakingParticipants = false;
	private listeners = new Set<() => void>();

	constructor() {
		makeAutoObservable<this, 'listeners' | 'notifyListeners'>(
			this,
			{
				listeners: false,
				getInputDeviceId: false,
				getOutputDeviceId: false,
				getVideoDeviceId: false,
				getInputVolume: false,
				getOutputVolume: false,
				getEchoCancellation: false,
				getNoiseSuppression: false,
				getAutoGainControl: false,
				getDeepFilterNoiseSuppression: false,
				getDeepFilterNoiseSuppressionLevel: false,
				getVoiceProcessingMode: false,
				getCameraResolution: false,
				getMirrorCamera: false,
				getScreenshareResolution: false,
				getVideoFrameRate: false,
				getStreamingMode: false,
				getHideStreamPreview: false,
				getMuteStreamAudio: false,
				getShareAppAudio: false,
				getShareDesktopAudio: false,
				getShareDeviceAudio: false,
				getScreenShareAudioDeviceId: false,
				getEffectiveScreenShareAudioDeviceId: false,
				getBackgroundImageId: false,
				getBackgroundImages: false,
				getBackgroundBlurStrength: false,
				getShowGridView: false,
				getShowMyOwnCamera: false,
				getShowMyOwnScreenShare: false,
				getShowNonVideoParticipants: false,
				getShowParticipantsCarousel: false,
				getShowVoiceConnectionAvatarStack: false,
				getShowVoiceConnectionId: false,
				getShowConnectionVolumeControls: false,
				getDisablePictureInPicturePopoutScreenShare: false,
				getPauseOwnScreenSharePreviewOnUnfocus: false,
				getPreferredVideoCodec: false,
				getEmulatedDecodeVideoCodecCap: false,
				getPreferredScreenShareCodec: false,
				getScreenShareContentHint: false,
				getScreenShareContentHintOverride: false,
				getScreenShareEncoderMode: false,
				getScreenShareSoftwareQuality: false,
				getScreenShareSoftwareQualityOverride: false,
				getScreenShareScalabilityMode: false,
				getScreenShareScalabilityModeOverride: false,
				getScreenShareBackupCodecMode: false,
				getScreenShareBackupCodecModeOverride: false,
				getScreenShareMaxBitrateMbps: false,
				getScreenShareMaxBitrateBpsOverride: false,
				getAdaptiveScreenShareQuality: false,
				getVadThreshold: false,
				getVadAutoSensitivity: false,
				getVadEnhanced: false,
				getLinuxAudioCaptureWorkaround: false,
				getLinuxAudioCaptureOnlySpeakers: false,
				getLinuxAudioCaptureOnlyDefaultSpeakers: false,
				getLinuxAudioCaptureIgnoreInputMedia: false,
				getLinuxAudioCaptureIgnoreVirtual: false,
				getLinuxAudioCaptureIgnoreDevices: false,
				getLinuxAudioCaptureGranularSelect: false,
				getLinuxAudioCaptureDeviceSelect: false,
				getScreenShareAudioSourceMode: false,
				getScreenShareAudioIncludeSources: false,
				getScreenShareAudioExcludeSources: false,
				getOpenH264Enabled: false,
				getLastScreenShareSource: false,
				getPrioritizeSpeakingParticipants: false,
				notifyListeners: false,
			},
			{autoBind: true},
		);
		this.initPersistence();
	}

	private migratePersistedSettings(): void {
		try {
			const raw = AppStorage.getItem('VoiceSettings');
			const parsed = parseVoiceSettingsStorage(raw);
			let changed = false;
			if (raw) {
				changed = applyLegacyVenmicKeyMigration(parsed);
			}
			changed = applyPiPPopoutDefaultsMigration(parsed) || changed;
			changed = applyAdaptiveScreenShareQualityMigrationV2(parsed) || changed;
			changed = applyScreenShareAudioConsentMigrationV1(parsed) || changed;
			changed = applyStreamingModeDefaultMigrationV1(parsed) || changed;
			if (changed) {
				AppStorage.setItem('VoiceSettings', JSON.stringify(parsed));
			}
		} catch (error) {
			logger.warn('Failed to migrate persisted voice settings:', error);
		}
	}

	private async initPersistence(): Promise<void> {
		this.migratePersistedSettings();
		await makePersistent(this, 'VoiceSettings', [
			'inputDeviceId',
			'outputDeviceId',
			'videoDeviceId',
			'inputVolume',
			'outputVolume',
			'echoCancellation',
			'noiseSuppression',
			'autoGainControl',
			'deepFilterNoiseSuppressionPrefV2',
			'deepFilterNoiseSuppressionLevelPrefV2',
			'voiceProcessingMode',
			'voiceProcessingModeByDeviceLabel',
			'cameraResolution',
			'mirrorCamera',
			'screenshareResolution',
			'videoFrameRate',
			'streamingMode',
			'streamingModeDefaultMigratedV1',
			'hideStreamPreview',
			'muteStreamAudio',
			'shareAppAudio',
			'shareDesktopAudio',
			'shareDeviceAudio',
			'screenShareAudioDeviceId',
			'screenShareAudioConsentMigratedV1',
			'backgroundImageId',
			'backgroundImages',
			'backgroundBlurStrength',
			'showGridView',
			'showMyOwnCamera',
			'showMyOwnScreenShare',
			'showParticipantsCarousel',
			'showVoiceConnectionAvatarStack',
			'showVoiceConnectionIdPrefV2',
			'showConnectionVolumeControls',
			'pauseOwnScreenSharePreviewOnUnfocusPrefV2',
			'disablePictureInPicturePopoutScreenShare',
			'preferredVideoCodec',
			'preferredScreenShareCodec',
			'emulatedDecodeVideoCodecCap',
			'screenShareContentHintPrefV2',
			'screenShareEncoderModePrefV2',
			'screenShareSoftwareQualityPrefV2',
			'screenShareScalabilityModePrefV2',
			'screenShareBackupCodecModePrefV2',
			'screenShareMaxBitrateMbpsPrefV2',
			'adaptiveScreenShareQualityPrefV2',
			'vadThreshold',
			'vadAutoSensitivity',
			'vadEnhanced',
			'linuxAudioCaptureWorkaround',
			'linuxAudioCaptureOnlySpeakers',
			'linuxAudioCaptureOnlyDefaultSpeakers',
			'linuxAudioCaptureIgnoreInputMedia',
			'linuxAudioCaptureIgnoreVirtual',
			'linuxAudioCaptureIgnoreDevices',
			'linuxAudioCaptureGranularSelect',
			'linuxAudioCaptureDeviceSelect',
			'screenShareAudioSourceMode',
			'screenShareAudioIncludeSources',
			'screenShareAudioExcludeSources',
			'openH264Enabled',
			'lastScreenShareSource',
			'prioritizeSpeakingParticipants',
		]);
		this.updateSettings({});
	}

	get showVoiceConnectionId(): boolean {
		return this.showVoiceConnectionIdPrefV2;
	}

	set showVoiceConnectionId(value: boolean) {
		this.showVoiceConnectionIdPrefV2 = value;
	}

	get pauseOwnScreenSharePreviewOnUnfocus(): boolean {
		return this.pauseOwnScreenSharePreviewOnUnfocusPrefV2;
	}

	set pauseOwnScreenSharePreviewOnUnfocus(value: boolean) {
		this.pauseOwnScreenSharePreviewOnUnfocusPrefV2 = value;
	}

	get deepFilterNoiseSuppression(): boolean {
		return this.deepFilterNoiseSuppressionPrefV2;
	}

	set deepFilterNoiseSuppression(value: boolean) {
		this.deepFilterNoiseSuppressionPrefV2 = value;
	}

	get deepFilterNoiseSuppressionLevel(): number {
		return this.deepFilterNoiseSuppressionLevelPrefV2;
	}

	set deepFilterNoiseSuppressionLevel(value: number) {
		this.deepFilterNoiseSuppressionLevelPrefV2 = Math.max(0, Math.min(100, value));
	}

	get screenShareContentHint(): ScreenShareContentHint {
		return this.screenShareContentHintPrefV2;
	}

	set screenShareContentHint(value: ScreenShareContentHint) {
		this.screenShareContentHintPrefV2 = value;
	}

	get screenShareEncoderMode(): ScreenShareEncoderMode {
		return this.screenShareEncoderModePrefV2;
	}

	set screenShareEncoderMode(value: ScreenShareEncoderMode) {
		this.screenShareEncoderModePrefV2 = value;
	}

	get screenShareSoftwareQuality(): ScreenShareSoftwareQuality {
		return this.screenShareSoftwareQualityPrefV2;
	}

	set screenShareSoftwareQuality(value: ScreenShareSoftwareQuality) {
		this.screenShareSoftwareQualityPrefV2 = value;
	}

	get screenShareScalabilityMode(): ScreenShareScalabilityModePreference {
		return this.screenShareScalabilityModePrefV2;
	}

	set screenShareScalabilityMode(value: ScreenShareScalabilityModePreference) {
		this.screenShareScalabilityModePrefV2 = value;
	}

	get screenShareBackupCodecMode(): ScreenShareBackupCodecMode {
		return this.screenShareBackupCodecModePrefV2;
	}

	set screenShareBackupCodecMode(value: ScreenShareBackupCodecMode) {
		this.screenShareBackupCodecModePrefV2 = value;
	}

	get screenShareMaxBitrateMbps(): number {
		return this.screenShareMaxBitrateMbpsPrefV2;
	}

	set screenShareMaxBitrateMbps(value: number) {
		this.screenShareMaxBitrateMbpsPrefV2 = value;
	}

	get adaptiveScreenShareQuality(): boolean {
		return this.adaptiveScreenShareQualityPrefV2;
	}

	set adaptiveScreenShareQuality(value: boolean) {
		this.adaptiveScreenShareQualityPrefV2 = value;
	}

	handleConnectionOpen(user: UserPrivate): void {
		if (this.isUserPremium(user.premium_type)) {
			return;
		}
		if (!this.hasHigherVideoQuality()) {
			this.sanitizePremiumSettings();
		}
	}

	handleUserUpdate(user: Partial<UserPrivate>): void {
		if (user.premium_type === undefined) {
			return;
		}
		if (this.isUserPremium(user.premium_type)) {
			return;
		}
		if (!this.hasHigherVideoQuality()) {
			this.sanitizePremiumSettings();
		}
	}

	private isUserPremium(premiumType: number | null | undefined): boolean {
		return premiumType != null && premiumType > 0;
	}

	private sanitizePremiumSettings(): void {
		if (
			this.screenshareResolution === 'high' ||
			this.screenshareResolution === 'ultra' ||
			this.screenshareResolution === 'source'
		) {
			this.screenshareResolution = 'medium';
		}
		if (this.cameraResolution === 'high') {
			this.cameraResolution = 'medium';
		}
		if (this.videoFrameRate > 30) {
			this.videoFrameRate = 30;
		}
	}

	private hasHigherVideoQuality(): boolean {
		const featureFlag = isLimitToggleEnabled(
			{
				feature_higher_video_quality: LimitResolver.resolve({
					key: 'feature_higher_video_quality',
					fallback: 0,
				}),
			},
			'feature_higher_video_quality',
		);
		return featureFlag;
	}

	getInputDeviceId(): string {
		return this.inputDeviceId;
	}

	getOutputDeviceId(): string {
		return this.outputDeviceId;
	}

	getVideoDeviceId(): string {
		return this.videoDeviceId;
	}

	getInputVolume(): number {
		return this.inputVolume;
	}

	getOutputVolume(): number {
		return this.outputVolume;
	}

	getEchoCancellation(): boolean {
		return this.echoCancellation;
	}

	getNoiseSuppression(): boolean {
		return this.noiseSuppression;
	}

	getAutoGainControl(): boolean {
		return this.autoGainControl;
	}

	getDeepFilterNoiseSuppression(): boolean {
		return this.deepFilterNoiseSuppression;
	}

	getDeepFilterNoiseSuppressionLevel(): number {
		return this.deepFilterNoiseSuppressionLevel;
	}

	getVoiceProcessingMode(): VoiceProcessingMode {
		return this.voiceProcessingMode;
	}

	getVoiceProcessingModeForDeviceLabel(label: string | null | undefined): VoiceProcessingMode {
		if (label && Object.hasOwn(this.voiceProcessingModeByDeviceLabel, label)) {
			return this.voiceProcessingModeByDeviceLabel[label];
		}
		return this.voiceProcessingMode;
	}

	hasVoiceProcessingModeOverrideForDeviceLabel(label: string | null | undefined): boolean {
		return Boolean(label) && Object.hasOwn(this.voiceProcessingModeByDeviceLabel, label as string);
	}

	setVoiceProcessingModeForDeviceLabel(label: string, mode: VoiceProcessingMode): void {
		const next: Record<string, VoiceProcessingMode> = {};
		for (const [key, value] of Object.entries(this.voiceProcessingModeByDeviceLabel)) {
			if (key === label) continue;
			next[key] = value;
		}
		next[label] = mode;
		const keys = Object.keys(next);
		if (keys.length > MAX_VOICE_PROCESSING_DEVICE_OVERRIDES) {
			const evict = keys.slice(0, keys.length - MAX_VOICE_PROCESSING_DEVICE_OVERRIDES);
			for (const key of evict) delete next[key];
		}
		this.voiceProcessingModeByDeviceLabel = next;
		this.notifyListeners();
	}

	clearVoiceProcessingModeForDeviceLabel(label: string): void {
		if (!Object.hasOwn(this.voiceProcessingModeByDeviceLabel, label)) return;
		const next = {...this.voiceProcessingModeByDeviceLabel};
		delete next[label];
		this.voiceProcessingModeByDeviceLabel = next;
		this.notifyListeners();
	}

	getCameraResolution(): CameraResolution {
		return this.cameraResolution;
	}

	getMirrorCamera(): boolean {
		return this.mirrorCamera;
	}

	getScreenshareResolution(): ScreenshareResolution {
		return this.screenshareResolution;
	}

	getVideoFrameRate(): number {
		return this.videoFrameRate;
	}

	getStreamingMode(): StreamingMode {
		return this.streamingMode;
	}

	getHideStreamPreview(): boolean {
		return this.hideStreamPreview;
	}

	getMuteStreamAudio(): boolean {
		return this.muteStreamAudio;
	}

	getShareAppAudio(): boolean {
		return this.shareAppAudio;
	}

	getShareDesktopAudio(): boolean {
		return this.shareDesktopAudio;
	}

	getShareDeviceAudio(): boolean {
		return this.shareDeviceAudio;
	}

	getScreenShareAudioDeviceId(): string {
		return this.screenShareAudioDeviceId;
	}

	getEffectiveScreenShareAudioDeviceId(): string {
		if (this.screenShareAudioDeviceId && this.screenShareAudioDeviceId !== 'default') {
			return this.screenShareAudioDeviceId;
		}
		return this.inputDeviceId || 'default';
	}

	getBackgroundImageId(): string {
		if (!areVoiceBackgroundsAvailable()) return NONE_BACKGROUND_ID;
		return this.backgroundImageId;
	}

	getBackgroundImages(): ReadonlyArray<BackgroundImage> {
		return this.backgroundImages;
	}

	getBackgroundBlurStrength(): number {
		return this.backgroundBlurStrength;
	}

	getShowGridView(): boolean {
		return this.showGridView;
	}

	getShowMyOwnCamera(): boolean {
		return this.showMyOwnCamera;
	}

	getShowMyOwnScreenShare(): boolean {
		return this.showMyOwnScreenShare;
	}

	getShowNonVideoParticipants(): boolean {
		return this.showNonVideoParticipants;
	}

	getShowParticipantsCarousel(): boolean {
		return this.showParticipantsCarousel;
	}

	getShowVoiceConnectionAvatarStack(): boolean {
		return this.showVoiceConnectionAvatarStack;
	}

	getShowVoiceConnectionId(): boolean {
		return this.showVoiceConnectionId;
	}

	getShowConnectionVolumeControls(): boolean {
		return this.showConnectionVolumeControls;
	}

	getDisablePictureInPicturePopoutScreenShare(): boolean {
		return this.disablePictureInPicturePopoutScreenShare;
	}

	getPauseOwnScreenSharePreviewOnUnfocus(): boolean {
		return this.pauseOwnScreenSharePreviewOnUnfocus;
	}

	getPreferredVideoCodec(): CodecPreference {
		return this.preferredVideoCodec;
	}

	getEmulatedDecodeVideoCodecCap(): CodecPreference {
		return this.emulatedDecodeVideoCodecCap;
	}

	getPreferredScreenShareCodec(): CodecPreference {
		return this.preferredScreenShareCodec;
	}

	getScreenShareContentHint(): ScreenShareContentHint {
		return this.screenShareContentHint;
	}

	getScreenShareContentHintOverride(): Exclude<ScreenShareContentHint, 'auto'> | undefined {
		const hint = this.screenShareContentHint;
		return hint === 'auto' ? undefined : hint;
	}

	getScreenShareEncoderMode(): ScreenShareEncoderMode {
		return this.screenShareEncoderMode;
	}

	getScreenShareSoftwareQuality(): ScreenShareSoftwareQuality {
		return this.screenShareSoftwareQuality;
	}

	getScreenShareSoftwareQualityOverride(): ScreenShareSoftwareQuality | undefined {
		return this.screenShareSoftwareQuality === DEFAULT_SCREEN_SHARE_SOFTWARE_QUALITY
			? undefined
			: this.screenShareSoftwareQuality;
	}

	getScreenShareScalabilityMode(): ScreenShareScalabilityModePreference {
		return this.screenShareScalabilityMode;
	}

	getScreenShareScalabilityModeOverride(): Exclude<ScreenShareScalabilityModePreference, 'auto'> | undefined {
		const mode = this.screenShareScalabilityMode;
		return mode === 'auto' ? undefined : mode;
	}

	getScreenShareBackupCodecMode(): ScreenShareBackupCodecMode {
		return this.screenShareBackupCodecMode;
	}

	getScreenShareBackupCodecModeOverride(): Exclude<ScreenShareBackupCodecMode, 'off'> | undefined {
		const mode = this.screenShareBackupCodecMode;
		return mode === 'off' ? undefined : mode;
	}

	getScreenShareMaxBitrateMbps(): number {
		return this.screenShareMaxBitrateMbps;
	}

	getScreenShareMaxBitrateBpsOverride(): number | undefined {
		return this.screenShareMaxBitrateMbps === DEFAULT_SCREEN_SHARE_MAX_BITRATE_MBPS
			? undefined
			: this.screenShareMaxBitrateMbps * 1000000;
	}

	getAdaptiveScreenShareQuality(): boolean {
		return this.adaptiveScreenShareQuality;
	}

	getVadThreshold(): number {
		return this.vadThreshold;
	}

	getVadAutoSensitivity(): boolean {
		return this.vadAutoSensitivity;
	}

	getVadEnhanced(): boolean {
		return this.vadEnhanced;
	}

	getLinuxAudioCaptureWorkaround(): boolean {
		return this.linuxAudioCaptureWorkaround;
	}

	getLinuxAudioCaptureOnlySpeakers(): boolean {
		return this.linuxAudioCaptureOnlySpeakers;
	}

	getLinuxAudioCaptureOnlyDefaultSpeakers(): boolean {
		return this.linuxAudioCaptureOnlyDefaultSpeakers;
	}

	getLinuxAudioCaptureIgnoreInputMedia(): boolean {
		return this.linuxAudioCaptureIgnoreInputMedia;
	}

	getLinuxAudioCaptureIgnoreVirtual(): boolean {
		return this.linuxAudioCaptureIgnoreVirtual;
	}

	getLinuxAudioCaptureIgnoreDevices(): boolean {
		return this.linuxAudioCaptureIgnoreDevices;
	}

	getLinuxAudioCaptureGranularSelect(): boolean {
		return this.linuxAudioCaptureGranularSelect;
	}

	getLinuxAudioCaptureDeviceSelect(): boolean {
		return this.linuxAudioCaptureDeviceSelect;
	}

	getScreenShareAudioSourceMode(): 'none' | 'system' | 'specific' {
		return this.screenShareAudioSourceMode;
	}

	getScreenShareAudioIncludeSources(): Array<Record<string, string>> {
		return this.screenShareAudioIncludeSources;
	}

	getScreenShareAudioExcludeSources(): Array<Record<string, string>> {
		return this.screenShareAudioExcludeSources;
	}

	getOpenH264Enabled(): boolean {
		return this.openH264Enabled;
	}

	getLastScreenShareSource(): LastScreenShareSource | null {
		return this.lastScreenShareSource;
	}

	getPrioritizeSpeakingParticipants(): boolean {
		return this.prioritizeSpeakingParticipants;
	}

	setPrioritizeSpeakingParticipants(enabled: boolean): void {
		this.prioritizeSpeakingParticipants = enabled;
		this.notifyListeners();
	}

	setLastScreenShareSource(source: LastScreenShareSource | null): void {
		this.lastScreenShareSource = validateLastScreenShareSource(source);
		this.notifyListeners();
	}

	updateSettings(data: VoiceSettingsUpdate): void {
		const validated = this.validateSettings(data);
		if (validated.inputDeviceId !== undefined) this.inputDeviceId = validated.inputDeviceId;
		if (validated.outputDeviceId !== undefined) this.outputDeviceId = validated.outputDeviceId;
		if (validated.videoDeviceId !== undefined) this.videoDeviceId = validated.videoDeviceId;
		if (validated.inputVolume !== undefined) this.inputVolume = validated.inputVolume;
		if (validated.outputVolume !== undefined) this.outputVolume = validated.outputVolume;
		if (validated.echoCancellation !== undefined) this.echoCancellation = validated.echoCancellation;
		if (validated.noiseSuppression !== undefined) this.noiseSuppression = validated.noiseSuppression;
		if (validated.autoGainControl !== undefined) this.autoGainControl = validated.autoGainControl;
		if (validated.deepFilterNoiseSuppression !== undefined)
			this.deepFilterNoiseSuppression = validated.deepFilterNoiseSuppression;
		if (validated.deepFilterNoiseSuppressionLevel !== undefined)
			this.deepFilterNoiseSuppressionLevel = validated.deepFilterNoiseSuppressionLevel;
		if (validated.voiceProcessingMode !== undefined) this.voiceProcessingMode = validated.voiceProcessingMode;
		if (validated.cameraResolution !== undefined) this.cameraResolution = validated.cameraResolution;
		if (validated.mirrorCamera !== undefined) this.mirrorCamera = validated.mirrorCamera;
		if (validated.screenshareResolution !== undefined) this.screenshareResolution = validated.screenshareResolution;
		if (validated.videoFrameRate !== undefined) this.videoFrameRate = validated.videoFrameRate;
		if (validated.streamingMode !== undefined) this.streamingMode = validated.streamingMode;
		if (validated.hideStreamPreview !== undefined) this.hideStreamPreview = validated.hideStreamPreview;
		if (validated.muteStreamAudio !== undefined) this.muteStreamAudio = validated.muteStreamAudio;
		if (validated.shareAppAudio !== undefined) this.shareAppAudio = validated.shareAppAudio;
		if (validated.shareDesktopAudio !== undefined) this.shareDesktopAudio = validated.shareDesktopAudio;
		if (validated.shareDeviceAudio !== undefined) this.shareDeviceAudio = validated.shareDeviceAudio;
		if (validated.screenShareAudioDeviceId !== undefined)
			this.screenShareAudioDeviceId = validated.screenShareAudioDeviceId;
		if (validated.backgroundImageId !== undefined) this.backgroundImageId = validated.backgroundImageId;
		if (validated.backgroundImages !== undefined) this.backgroundImages = validated.backgroundImages;
		if (validated.backgroundBlurStrength !== undefined) this.backgroundBlurStrength = validated.backgroundBlurStrength;
		if (validated.showGridView !== undefined) this.showGridView = validated.showGridView;
		if (validated.showMyOwnCamera !== undefined) this.showMyOwnCamera = validated.showMyOwnCamera;
		if (validated.showMyOwnScreenShare !== undefined) this.showMyOwnScreenShare = validated.showMyOwnScreenShare;
		if (validated.showNonVideoParticipants !== undefined)
			this.showNonVideoParticipants = validated.showNonVideoParticipants;
		if (validated.showParticipantsCarousel !== undefined)
			this.showParticipantsCarousel = validated.showParticipantsCarousel;
		if (validated.showVoiceConnectionAvatarStack !== undefined)
			this.showVoiceConnectionAvatarStack = validated.showVoiceConnectionAvatarStack;
		if (validated.showVoiceConnectionId !== undefined) this.showVoiceConnectionId = validated.showVoiceConnectionId;
		if (validated.showConnectionVolumeControls !== undefined)
			this.showConnectionVolumeControls = validated.showConnectionVolumeControls;
		if (validated.pauseOwnScreenSharePreviewOnUnfocus !== undefined)
			this.pauseOwnScreenSharePreviewOnUnfocus = validated.pauseOwnScreenSharePreviewOnUnfocus;
		if (validated.disablePictureInPicturePopoutScreenShare !== undefined)
			this.disablePictureInPicturePopoutScreenShare = validated.disablePictureInPicturePopoutScreenShare;
		if (validated.preferredVideoCodec !== undefined) this.preferredVideoCodec = validated.preferredVideoCodec;
		if (validated.preferredScreenShareCodec !== undefined)
			this.preferredScreenShareCodec = validated.preferredScreenShareCodec;
		if (validated.emulatedDecodeVideoCodecCap !== undefined)
			this.emulatedDecodeVideoCodecCap = validated.emulatedDecodeVideoCodecCap;
		if (validated.screenShareContentHint !== undefined) this.screenShareContentHint = validated.screenShareContentHint;
		if (validated.screenShareEncoderMode !== undefined) this.screenShareEncoderMode = validated.screenShareEncoderMode;
		if (validated.screenShareSoftwareQuality !== undefined)
			this.screenShareSoftwareQuality = validated.screenShareSoftwareQuality;
		if (validated.screenShareScalabilityMode !== undefined)
			this.screenShareScalabilityMode = validated.screenShareScalabilityMode;
		if (validated.screenShareBackupCodecMode !== undefined)
			this.screenShareBackupCodecMode = validated.screenShareBackupCodecMode;
		if (validated.screenShareMaxBitrateMbps !== undefined)
			this.screenShareMaxBitrateMbps = validated.screenShareMaxBitrateMbps;
		if (validated.adaptiveScreenShareQuality !== undefined)
			this.adaptiveScreenShareQuality = validated.adaptiveScreenShareQuality;
		if (validated.vadThreshold !== undefined) this.vadThreshold = validated.vadThreshold;
		if (validated.vadAutoSensitivity !== undefined) this.vadAutoSensitivity = validated.vadAutoSensitivity;
		if (validated.vadEnhanced !== undefined) this.vadEnhanced = validated.vadEnhanced;
		if (validated.linuxAudioCaptureWorkaround !== undefined)
			this.linuxAudioCaptureWorkaround = validated.linuxAudioCaptureWorkaround;
		if (validated.linuxAudioCaptureOnlySpeakers !== undefined)
			this.linuxAudioCaptureOnlySpeakers = validated.linuxAudioCaptureOnlySpeakers;
		if (validated.linuxAudioCaptureOnlyDefaultSpeakers !== undefined)
			this.linuxAudioCaptureOnlyDefaultSpeakers = validated.linuxAudioCaptureOnlyDefaultSpeakers;
		if (validated.linuxAudioCaptureIgnoreInputMedia !== undefined)
			this.linuxAudioCaptureIgnoreInputMedia = validated.linuxAudioCaptureIgnoreInputMedia;
		if (validated.linuxAudioCaptureIgnoreVirtual !== undefined)
			this.linuxAudioCaptureIgnoreVirtual = validated.linuxAudioCaptureIgnoreVirtual;
		if (validated.linuxAudioCaptureIgnoreDevices !== undefined)
			this.linuxAudioCaptureIgnoreDevices = validated.linuxAudioCaptureIgnoreDevices;
		if (validated.linuxAudioCaptureGranularSelect !== undefined)
			this.linuxAudioCaptureGranularSelect = validated.linuxAudioCaptureGranularSelect;
		if (validated.linuxAudioCaptureDeviceSelect !== undefined)
			this.linuxAudioCaptureDeviceSelect = validated.linuxAudioCaptureDeviceSelect;
		if (validated.screenShareAudioSourceMode !== undefined)
			this.screenShareAudioSourceMode = validated.screenShareAudioSourceMode;
		if (validated.screenShareAudioIncludeSources !== undefined)
			this.screenShareAudioIncludeSources = validated.screenShareAudioIncludeSources;
		if (validated.screenShareAudioExcludeSources !== undefined)
			this.screenShareAudioExcludeSources = validated.screenShareAudioExcludeSources;
		if (validated.openH264Enabled !== undefined) this.openH264Enabled = validated.openH264Enabled;
		if (validated.lastScreenShareSource !== undefined) this.lastScreenShareSource = validated.lastScreenShareSource;
		this.notifyListeners();
	}

	subscribe(listener: () => void): () => void {
		this.listeners.add(listener);
		return () => {
			this.listeners.delete(listener);
		};
	}

	private notifyListeners(): void {
		for (const listener of [...this.listeners]) {
			listener();
		}
	}

	private validateSettings(data: VoiceSettingsUpdate): VoiceSettingsUpdate {
		let voiceProcessingMode = data.voiceProcessingMode ?? this.voiceProcessingMode;
		const validVoiceProcessingModes: Array<VoiceProcessingMode> = ['voice', 'studio', 'custom'];
		if (!validVoiceProcessingModes.includes(voiceProcessingMode)) {
			voiceProcessingMode = DEFAULT_VOICE_PROCESSING_MODE;
		}
		let cameraResolution = data.cameraResolution ?? this.cameraResolution;
		let screenshareResolution = data.screenshareResolution ?? this.screenshareResolution;
		let videoFrameRate = clampVideoFrameRate(data.videoFrameRate ?? this.videoFrameRate);
		const streamingMode = validateStreamingMode(data.streamingMode ?? this.streamingMode);
		let backgroundImages = validateBackgroundImages(data.backgroundImages ?? this.backgroundImages);
		let backgroundImageId = data.backgroundImageId ?? this.backgroundImageId;
		const screenShareEncoderMode = validateScreenShareEncoderMode(
			data.screenShareEncoderMode ?? this.screenShareEncoderMode,
		);
		const screenShareSoftwareQuality = validateScreenShareSoftwareQuality(
			data.screenShareSoftwareQuality ?? this.screenShareSoftwareQuality,
		);
		const screenShareScalabilityMode = validateScreenShareScalabilityMode(
			data.screenShareScalabilityMode ?? this.screenShareScalabilityMode,
		);
		const screenShareBackupCodecMode = validateScreenShareBackupCodecMode(
			data.screenShareBackupCodecMode ?? this.screenShareBackupCodecMode,
		);
		const validCameraResolutions: Array<CameraResolution> = ['low', 'medium', 'high'];
		if (!validCameraResolutions.includes(cameraResolution)) {
			cameraResolution = 'medium';
		}
		const hasHigherQuality = this.hasHigherVideoQuality();
		const validScreenshareResolutions: Array<ScreenshareResolution> = [
			'low_240p',
			'low_480p',
			'medium',
			'high',
			'ultra',
			'source',
		];
		if (!validScreenshareResolutions.includes(screenshareResolution)) {
			screenshareResolution = 'medium';
		}
		if (!hasHigherQuality) {
			if (screenshareResolution === 'high' || screenshareResolution === 'ultra' || screenshareResolution === 'source') {
				screenshareResolution = 'medium';
			}
			if (cameraResolution === 'high') {
				cameraResolution = 'medium';
			}
			videoFrameRate = Math.min(30, videoFrameRate);
			if (backgroundImages.length > 3) {
				backgroundImages = backgroundImages.slice(0, 3);
			}
		}
		if (backgroundImageId !== NONE_BACKGROUND_ID && backgroundImageId !== BLUR_BACKGROUND_ID) {
			const imageExists = backgroundImages.some((img: BackgroundImage) => img.id === backgroundImageId);
			if (!imageExists) {
				backgroundImageId = NONE_BACKGROUND_ID;
			}
		}
		return {
			inputDeviceId: data.inputDeviceId ?? this.inputDeviceId,
			outputDeviceId: data.outputDeviceId ?? this.outputDeviceId,
			videoDeviceId: data.videoDeviceId ?? this.videoDeviceId,
			inputVolume: clampVoiceVolumePercent(data.inputVolume ?? this.inputVolume),
			outputVolume: clampVoiceVolumePercent(data.outputVolume ?? this.outputVolume),
			echoCancellation: data.echoCancellation ?? this.echoCancellation,
			noiseSuppression: data.noiseSuppression ?? this.noiseSuppression,
			autoGainControl: data.autoGainControl ?? this.autoGainControl,
			deepFilterNoiseSuppression: data.deepFilterNoiseSuppression ?? this.deepFilterNoiseSuppression,
			deepFilterNoiseSuppressionLevel: Math.max(
				0,
				Math.min(100, data.deepFilterNoiseSuppressionLevel ?? this.deepFilterNoiseSuppressionLevel),
			),
			voiceProcessingMode,
			cameraResolution,
			mirrorCamera: data.mirrorCamera ?? this.mirrorCamera,
			screenshareResolution,
			videoFrameRate: clampVideoFrameRate(videoFrameRate),
			streamingMode,
			hideStreamPreview: data.hideStreamPreview ?? this.hideStreamPreview,
			muteStreamAudio: data.muteStreamAudio ?? this.muteStreamAudio,
			shareAppAudio: data.shareAppAudio ?? this.shareAppAudio,
			shareDesktopAudio: data.shareDesktopAudio ?? this.shareDesktopAudio,
			shareDeviceAudio: data.shareDeviceAudio ?? this.shareDeviceAudio,
			screenShareAudioDeviceId: data.screenShareAudioDeviceId ?? this.screenShareAudioDeviceId,
			backgroundImageId,
			backgroundImages,
			backgroundBlurStrength: clampCameraEffectStrength(data.backgroundBlurStrength ?? this.backgroundBlurStrength),
			showGridView: data.showGridView ?? this.showGridView,
			showMyOwnCamera: data.showMyOwnCamera ?? this.showMyOwnCamera,
			showMyOwnScreenShare: data.showMyOwnScreenShare ?? this.showMyOwnScreenShare,
			showNonVideoParticipants: data.showNonVideoParticipants ?? this.showNonVideoParticipants,
			showParticipantsCarousel: data.showParticipantsCarousel ?? this.showParticipantsCarousel,
			showVoiceConnectionAvatarStack: data.showVoiceConnectionAvatarStack ?? this.showVoiceConnectionAvatarStack,
			showVoiceConnectionId: data.showVoiceConnectionId ?? this.showVoiceConnectionId,
			showConnectionVolumeControls:
				typeof data.showConnectionVolumeControls === 'boolean'
					? data.showConnectionVolumeControls
					: this.showConnectionVolumeControls === true,
			pauseOwnScreenSharePreviewOnUnfocus:
				data.pauseOwnScreenSharePreviewOnUnfocus ?? this.pauseOwnScreenSharePreviewOnUnfocus,
			disablePictureInPicturePopoutScreenShare:
				data.disablePictureInPicturePopoutScreenShare ?? this.disablePictureInPicturePopoutScreenShare,
			preferredVideoCodec: data.preferredVideoCodec ?? this.preferredVideoCodec,
			preferredScreenShareCodec: data.preferredScreenShareCodec ?? this.preferredScreenShareCodec,
			emulatedDecodeVideoCodecCap: data.emulatedDecodeVideoCodecCap ?? this.emulatedDecodeVideoCodecCap,
			screenShareContentHint: validateScreenShareContentHint(
				data.screenShareContentHint ?? this.screenShareContentHint,
			),
			screenShareEncoderMode,
			screenShareSoftwareQuality,
			screenShareScalabilityMode,
			screenShareBackupCodecMode,
			screenShareMaxBitrateMbps: Math.max(
				1,
				Math.min(50, data.screenShareMaxBitrateMbps ?? this.screenShareMaxBitrateMbps),
			),
			adaptiveScreenShareQuality: data.adaptiveScreenShareQuality ?? this.adaptiveScreenShareQuality,
			vadThreshold: Math.max(0, Math.min(100, data.vadThreshold ?? this.vadThreshold)),
			vadAutoSensitivity: data.vadAutoSensitivity ?? this.vadAutoSensitivity,
			vadEnhanced: data.vadEnhanced ?? this.vadEnhanced,
			linuxAudioCaptureWorkaround: data.linuxAudioCaptureWorkaround ?? this.linuxAudioCaptureWorkaround,
			linuxAudioCaptureOnlySpeakers: data.linuxAudioCaptureOnlySpeakers ?? this.linuxAudioCaptureOnlySpeakers,
			linuxAudioCaptureOnlyDefaultSpeakers:
				data.linuxAudioCaptureOnlyDefaultSpeakers ?? this.linuxAudioCaptureOnlyDefaultSpeakers,
			linuxAudioCaptureIgnoreInputMedia:
				data.linuxAudioCaptureIgnoreInputMedia ?? this.linuxAudioCaptureIgnoreInputMedia,
			linuxAudioCaptureIgnoreVirtual: data.linuxAudioCaptureIgnoreVirtual ?? this.linuxAudioCaptureIgnoreVirtual,
			linuxAudioCaptureIgnoreDevices: data.linuxAudioCaptureIgnoreDevices ?? this.linuxAudioCaptureIgnoreDevices,
			linuxAudioCaptureGranularSelect: data.linuxAudioCaptureGranularSelect ?? this.linuxAudioCaptureGranularSelect,
			linuxAudioCaptureDeviceSelect: data.linuxAudioCaptureDeviceSelect ?? this.linuxAudioCaptureDeviceSelect,
			screenShareAudioSourceMode: validateAudioSourceMode(
				data.screenShareAudioSourceMode ?? this.screenShareAudioSourceMode,
			),
			screenShareAudioIncludeSources:
				validateSourceList(data.screenShareAudioIncludeSources) ?? this.screenShareAudioIncludeSources,
			screenShareAudioExcludeSources:
				validateSourceList(data.screenShareAudioExcludeSources) ?? this.screenShareAudioExcludeSources,
			openH264Enabled: data.openH264Enabled ?? this.openH264Enabled,
			lastScreenShareSource:
				data.lastScreenShareSource === undefined
					? this.lastScreenShareSource
					: validateLastScreenShareSource(data.lastScreenShareSource),
		};
	}
}

function validateLastScreenShareSource(value: unknown): LastScreenShareSource | null {
	if (value === null) return null;
	if (!isRecord(value)) return null;
	const kind = value.kind;
	if (kind !== 'app' && kind !== 'display' && kind !== 'device' && kind !== 'game') return null;
	const sourceId = value.sourceId;
	if (sourceId !== null && (typeof sourceId !== 'string' || sourceId.length > 256)) return null;
	const title = typeof value.title === 'string' ? value.title.trim().slice(0, 256) : '';
	if (!title) return null;
	const updatedAt =
		typeof value.updatedAt === 'number' && Number.isFinite(value.updatedAt) ? value.updatedAt : Date.now();
	return {
		kind,
		sourceId,
		title,
		updatedAt,
	};
}

function validateScreenShareEncoderMode(mode: unknown): ScreenShareEncoderMode {
	return mode === 'hardware' || mode === 'software' || mode === 'auto' ? mode : DEFAULT_SCREEN_SHARE_ENCODER_MODE;
}

function validateScreenShareSoftwareQuality(mode: unknown): ScreenShareSoftwareQuality {
	return mode === 'realtime' || mode === 'quality' || mode === 'balanced'
		? mode
		: DEFAULT_SCREEN_SHARE_SOFTWARE_QUALITY;
}

function validateScreenShareScalabilityMode(mode: unknown): ScreenShareScalabilityModePreference {
	return mode === 'single_layer' || mode === 'temporal' || mode === 'spatial' || mode === 'auto'
		? mode
		: DEFAULT_SCREEN_SHARE_SCALABILITY_MODE;
}

function validateScreenShareBackupCodecMode(mode: unknown): ScreenShareBackupCodecMode {
	return mode === 'h264_simulcast' || mode === 'off' ? mode : DEFAULT_SCREEN_SHARE_BACKUP_CODEC_MODE;
}

function validateScreenShareContentHint(hint: unknown): ScreenShareContentHint {
	return hint === 'detail' || hint === 'motion' || hint === 'text' || hint === 'auto'
		? hint
		: DEFAULT_SCREEN_SHARE_CONTENT_HINT;
}

function validateStreamingMode(mode: unknown): StreamingMode {
	return mode === 'gaming' || mode === 'screenshare' || mode === 'custom' ? mode : 'screenshare';
}

function validateAudioSourceMode(mode: unknown): 'none' | 'system' | 'specific' {
	return mode === 'none' || mode === 'system' || mode === 'specific' ? mode : 'system';
}

function validateSourceList(value: unknown): Array<Record<string, string>> | undefined {
	if (!Array.isArray(value)) return undefined;
	return value
		.filter((entry): entry is Record<string, unknown> => Boolean(entry) && typeof entry === 'object')
		.map((entry) => {
			const sanitized: Record<string, string> = {};
			for (const [key, val] of Object.entries(entry)) {
				if (typeof key === 'string' && typeof val === 'string') {
					sanitized[key] = val;
				}
			}
			return sanitized;
		})
		.filter((entry) => Object.keys(entry).length > 0);
}

export default new VoiceSettings();
