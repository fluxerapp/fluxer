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

import {makeAutoObservable} from 'mobx';
import {makePersistent} from '~/lib/MobXPersistence';
import {type UserPrivate, UserRecord} from '~/records/UserRecord';

interface BackgroundImage {
	id: string;
	createdAt: number;
}

export const NONE_BACKGROUND_ID = 'none';
export const BLUR_BACKGROUND_ID = 'blur';

export type CameraResolution = 'low' | 'medium' | 'high';
export type ScreenshareResolution = 'low' | 'medium' | 'high' | 'ultra' | '4k';

type VoiceSettingsUpdate = Partial<{
	inputDeviceId: string;
	outputDeviceId: string;
	videoDeviceId: string;
	inputVolume: number;
	outputVolume: number;
	echoCancellation: boolean;
	noiseSuppression: boolean;
	autoGainControl: boolean;
	cameraResolution: CameraResolution;
	screenshareResolution: ScreenshareResolution;
	videoFrameRate: number;
	backgroundImageId: string;
	backgroundImages: Array<BackgroundImage>;
	showGridView: boolean;
	showMyOwnCamera: boolean;
	showNonVideoParticipants: boolean;
	showParticipantsCarousel: boolean;
}>;

class VoiceSettingsStore {
	inputDeviceId = 'default';
	outputDeviceId = 'default';
	videoDeviceId = 'default';
	inputVolume = 100;
	outputVolume = 100;
	echoCancellation = true;
	noiseSuppression = true;
	autoGainControl = true;
	cameraResolution: CameraResolution = 'medium';
	screenshareResolution: ScreenshareResolution = 'medium';
	videoFrameRate = 30;
	backgroundImageId = NONE_BACKGROUND_ID;
	backgroundImages: Array<BackgroundImage> = [];
	showGridView = false;
	showMyOwnCamera = true;
	showNonVideoParticipants = true;
	showParticipantsCarousel = true;
	hasPremium = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'VoiceSettingsStore', [
			'inputDeviceId',
			'outputDeviceId',
			'videoDeviceId',
			'inputVolume',
			'outputVolume',
			'echoCancellation',
			'noiseSuppression',
			'autoGainControl',
			'cameraResolution',
			'screenshareResolution',
			'videoFrameRate',
			'backgroundImageId',
			'backgroundImages',
			'showGridView',
			'showMyOwnCamera',
			'showNonVideoParticipants',
			'showParticipantsCarousel',
		]);
	}

	getHasPremium(): boolean {
		return this.hasPremium;
	}

	handleConnectionOpen(user: UserPrivate): void {
		const wasPremium = this.hasPremium;
		const userRecord = new UserRecord(user);
		this.hasPremium = userRecord.isPremium();

		if (!this.hasPremium) {
			this.sanitizePremiumSettings();
		} else if (!wasPremium && this.hasPremium) {
			this.applyPremiumDefaults();
		}
	}

	handleUserUpdate(user: Partial<UserPrivate>): void {
		if (user.premium_type === undefined) {
			return;
		}

		const wasPremium = this.hasPremium;
		const userRecord = new UserRecord(user as UserPrivate);
		this.hasPremium = userRecord.isPremium();

		if (wasPremium && !this.hasPremium) {
			this.sanitizePremiumSettings();
		} else if (!wasPremium && this.hasPremium) {
			this.applyPremiumDefaults();
		}
	}

	private sanitizePremiumSettings(): void {
		if (
			this.screenshareResolution === 'high' ||
			this.screenshareResolution === 'ultra' ||
			this.screenshareResolution === '4k'
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

	private applyPremiumDefaults(): void {
		if (this.cameraResolution === 'medium') {
			this.cameraResolution = 'high';
		}

		if (this.screenshareResolution === 'medium') {
			this.screenshareResolution = 'high';
		}
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

	getCameraResolution(): CameraResolution {
		return this.cameraResolution;
	}

	getScreenshareResolution(): ScreenshareResolution {
		return this.screenshareResolution;
	}

	getVideoFrameRate(): number {
		return this.videoFrameRate;
	}

	getBackgroundImageId(): string {
		return this.backgroundImageId;
	}

	getBackgroundImages(): ReadonlyArray<BackgroundImage> {
		return this.backgroundImages;
	}

	getShowGridView(): boolean {
		return this.showGridView;
	}

	getShowMyOwnCamera(): boolean {
		return this.showMyOwnCamera;
	}

	getShowNonVideoParticipants(): boolean {
		return this.showNonVideoParticipants;
	}

	getShowParticipantsCarousel(): boolean {
		return this.showParticipantsCarousel;
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
		if (validated.cameraResolution !== undefined) this.cameraResolution = validated.cameraResolution;
		if (validated.screenshareResolution !== undefined) this.screenshareResolution = validated.screenshareResolution;
		if (validated.videoFrameRate !== undefined) this.videoFrameRate = validated.videoFrameRate;
		if (validated.backgroundImageId !== undefined) this.backgroundImageId = validated.backgroundImageId;
		if (validated.backgroundImages !== undefined) this.backgroundImages = validated.backgroundImages;
		if (validated.showGridView !== undefined) this.showGridView = validated.showGridView;
		if (validated.showMyOwnCamera !== undefined) this.showMyOwnCamera = validated.showMyOwnCamera;
		if (validated.showNonVideoParticipants !== undefined)
			this.showNonVideoParticipants = validated.showNonVideoParticipants;
		if (validated.showParticipantsCarousel !== undefined)
			this.showParticipantsCarousel = validated.showParticipantsCarousel;
	}

	private validateSettings(data: VoiceSettingsUpdate): VoiceSettingsUpdate {
		let cameraResolution = data.cameraResolution ?? this.cameraResolution;
		let screenshareResolution = data.screenshareResolution ?? this.screenshareResolution;
		let videoFrameRate = data.videoFrameRate ?? this.videoFrameRate;
		let backgroundImages = data.backgroundImages ?? this.backgroundImages;
		let backgroundImageId = data.backgroundImageId ?? this.backgroundImageId;

		const validCameraResolutions: Array<CameraResolution> = ['low', 'medium', 'high'];
		if (!validCameraResolutions.includes(cameraResolution)) {
			cameraResolution = 'medium';
		}

		if (!this.hasPremium) {
			if (screenshareResolution === 'high' || screenshareResolution === 'ultra' || screenshareResolution === '4k') {
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
			inputVolume: Math.max(0, Math.min(100, data.inputVolume ?? this.inputVolume)),
			outputVolume: Math.max(0, Math.min(100, data.outputVolume ?? this.outputVolume)),
			echoCancellation: data.echoCancellation ?? this.echoCancellation,
			noiseSuppression: data.noiseSuppression ?? this.noiseSuppression,
			autoGainControl: data.autoGainControl ?? this.autoGainControl,
			cameraResolution,
			screenshareResolution,
			videoFrameRate: Math.max(15, Math.min(60, videoFrameRate)),
			backgroundImageId,
			backgroundImages,
			showGridView: data.showGridView ?? this.showGridView,
			showMyOwnCamera: data.showMyOwnCamera ?? this.showMyOwnCamera,
			showNonVideoParticipants: data.showNonVideoParticipants ?? this.showNonVideoParticipants,
			showParticipantsCarousel: data.showParticipantsCarousel ?? this.showParticipantsCarousel,
		};
	}
}

export default new VoiceSettingsStore();
