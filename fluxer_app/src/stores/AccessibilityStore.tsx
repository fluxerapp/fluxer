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

import {makeAutoObservable, reaction} from 'mobx';
import {StickerAnimationOptions} from '~/Constants';
import {makePersistent} from '~/lib/MobXPersistence';
import {loadTheme, persistTheme} from '~/lib/themePersistence';
import MobileLayoutStore from '~/stores/MobileLayoutStore';

export enum ChannelTypingIndicatorMode {
	AVATARS = 0,
	INDICATOR_ONLY = 1,
	HIDDEN = 2,
}

export enum MediaDimensionSize {
	SMALL = 'small',
	LARGE = 'large',
}

export enum DMMessagePreviewMode {
	ALL = 0,
	UNREAD_ONLY = 1,
	NONE = 2,
}

export interface AccessibilitySettings {
	saturationFactor: number;
	alwaysUnderlineLinks: boolean;
	enableTextSelection: boolean;
	showMessageSendButton: boolean;
	showTextareaFocusRing: boolean;
	hideKeyboardHints: boolean;
	escapeExitsKeyboardMode: boolean;
	syncReducedMotionWithSystem: boolean;
	reducedMotionOverride: boolean | null;
	messageGroupSpacing: number;
	messageGutter: number;
	fontSize: number;
	showUserAvatarsInCompactMode: boolean;
	mobileStickerAnimationOverridden: boolean;
	mobileGifAutoPlayOverridden: boolean;
	mobileAnimateEmojiOverridden: boolean;
	mobileStickerAnimationValue: number;
	mobileGifAutoPlayValue: boolean;
	mobileAnimateEmojiValue: boolean;
	syncThemeAcrossDevices: boolean;
	localThemeOverride: string | null;
	showMessageDividers: boolean;
	autoSendTenorGifs: boolean;
	showGiftButton: boolean;
	showGifButton: boolean;
	showMemesButton: boolean;
	showStickersButton: boolean;
	showEmojiButton: boolean;
	showUploadButton: boolean;
	showMediaFavoriteButton: boolean;
	showMediaDownloadButton: boolean;
	showMediaDeleteButton: boolean;
	showSuppressEmbedsButton: boolean;
	showGifIndicator: boolean;
	showAttachmentExpiryIndicator: boolean;
	useBrowserLocaleForTimeFormat: boolean;
	channelTypingIndicatorMode: ChannelTypingIndicatorMode;
	showSelectedChannelTypingIndicator: boolean;
	showMessageActionBar: boolean;
	showMessageActionBarQuickReactions: boolean;
	showMessageActionBarShiftExpand: boolean;
	showMessageActionBarOnlyMoreButton: boolean;
	showDefaultEmojisInExpressionAutocomplete: boolean;
	showCustomEmojisInExpressionAutocomplete: boolean;
	showStickersInExpressionAutocomplete: boolean;
	showMemesInExpressionAutocomplete: boolean;
	attachmentMediaDimensionSize: MediaDimensionSize;
	embedMediaDimensionSize: MediaDimensionSize;
	voiceChannelJoinRequiresDoubleClick: boolean;
	customThemeCss: string | null;
	showFavorites: boolean;
	zoomLevel: number;
	dmMessagePreviewMode: DMMessagePreviewMode;
	enableTTSCommand: boolean;
	ttsRate: number;
}

const getDefaultDmMessagePreviewMode = (): DMMessagePreviewMode =>
	MobileLayoutStore.isMobileLayout() ? DMMessagePreviewMode.ALL : DMMessagePreviewMode.NONE;

class AccessibilityStore {
	saturationFactor = 1;
	alwaysUnderlineLinks = false;
	enableTextSelection = false;
	showMessageSendButton = true;
	showTextareaFocusRing = true;
	hideKeyboardHints = false;
	escapeExitsKeyboardMode = false;
	syncReducedMotionWithSystem = true;
	reducedMotionOverride: boolean | null = null;
	messageGroupSpacing = 16;
	messageGutter = 16;
	fontSize = 16;
	showUserAvatarsInCompactMode = false;
	mobileStickerAnimationOverridden = false;
	mobileGifAutoPlayOverridden = false;
	mobileAnimateEmojiOverridden = false;
	mobileStickerAnimationValue: number = StickerAnimationOptions.ANIMATE_ON_INTERACTION;
	mobileGifAutoPlayValue = false;
	mobileAnimateEmojiValue = true;
	syncThemeAcrossDevices = true;
	localThemeOverride: string | null = null;
	showMessageDividers = false;
	autoSendTenorGifs = true;
	showGiftButton = true;
	showGifButton = true;
	showMemesButton = true;
	showStickersButton = true;
	showEmojiButton = true;
	showUploadButton = true;
	showMediaFavoriteButton = true;
	showMediaDownloadButton = true;
	showMediaDeleteButton = true;
	showSuppressEmbedsButton = true;
	showGifIndicator = true;
	showAttachmentExpiryIndicator = true;
	useBrowserLocaleForTimeFormat = false;
	channelTypingIndicatorMode: ChannelTypingIndicatorMode = ChannelTypingIndicatorMode.AVATARS;
	showSelectedChannelTypingIndicator = false;
	showMessageActionBar = true;
	showMessageActionBarQuickReactions = true;
	showMessageActionBarShiftExpand = true;
	showMessageActionBarOnlyMoreButton = false;
	showDefaultEmojisInExpressionAutocomplete = true;
	showCustomEmojisInExpressionAutocomplete = true;
	showStickersInExpressionAutocomplete = true;
	showMemesInExpressionAutocomplete = true;
	attachmentMediaDimensionSize = MediaDimensionSize.LARGE;
	embedMediaDimensionSize = MediaDimensionSize.SMALL;
	voiceChannelJoinRequiresDoubleClick = false;
	systemReducedMotion = false;
	customThemeCss: string | null = null;
	showFavorites = true;
	zoomLevel = 1.0;
	dmMessagePreviewMode: DMMessagePreviewMode = getDefaultDmMessagePreviewMode();
	enableTTSCommand = true;
	ttsRate = 1.0;
	mediaQuery: MediaQueryList | null = null;

	constructor() {
		makeAutoObservable(this, {mediaQuery: false}, {autoBind: true});
		this.bootstrapThemeOverride();
		this.initPersistence();
		this.initializeMotionDetection();
	}

	private bootstrapThemeOverride(): void {
		void loadTheme().then((explicitTheme) => {
			if (explicitTheme) {
				this.syncThemeAcrossDevices = false;
				this.localThemeOverride = explicitTheme;
			}
		});
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'AccessibilityStore', [
			'saturationFactor',
			'alwaysUnderlineLinks',
			'enableTextSelection',
			'showMessageSendButton',
			'showTextareaFocusRing',
			'hideKeyboardHints',
			'escapeExitsKeyboardMode',
			'syncReducedMotionWithSystem',
			'reducedMotionOverride',
			'messageGroupSpacing',
			'messageGutter',
			'fontSize',
			'showUserAvatarsInCompactMode',
			'mobileStickerAnimationOverridden',
			'mobileGifAutoPlayOverridden',
			'mobileAnimateEmojiOverridden',
			'mobileStickerAnimationValue',
			'mobileGifAutoPlayValue',
			'mobileAnimateEmojiValue',
			'syncThemeAcrossDevices',
			'localThemeOverride',
			'showMessageDividers',
			'autoSendTenorGifs',
			'showGiftButton',
			'showGifButton',
			'showMemesButton',
			'showStickersButton',
			'showEmojiButton',
			'showUploadButton',
			'showMediaFavoriteButton',
			'showMediaDownloadButton',
			'showMediaDeleteButton',
			'showSuppressEmbedsButton',
			'showGifIndicator',
			'showAttachmentExpiryIndicator',
			'useBrowserLocaleForTimeFormat',
			'channelTypingIndicatorMode',
			'showSelectedChannelTypingIndicator',
			'showMessageActionBar',
			'showMessageActionBarQuickReactions',
			'showMessageActionBarShiftExpand',
			'showMessageActionBarOnlyMoreButton',
			'showDefaultEmojisInExpressionAutocomplete',
			'showCustomEmojisInExpressionAutocomplete',
			'showStickersInExpressionAutocomplete',
			'showMemesInExpressionAutocomplete',
			'attachmentMediaDimensionSize',
			'embedMediaDimensionSize',
			'voiceChannelJoinRequiresDoubleClick',
			'customThemeCss',
			'showFavorites',
			'zoomLevel',
			'dmMessagePreviewMode',
			'enableTTSCommand',
			'ttsRate',
		]);
	}

	private initializeMotionDetection() {
		if (window.matchMedia) {
			this.mediaQuery = window.matchMedia('(prefers-reduced-motion: reduce)');
			this.systemReducedMotion = this.mediaQuery.matches;
			this.mediaQuery.addEventListener('change', this.handleSystemMotionChange);
		}
	}

	private handleSystemMotionChange = (event: MediaQueryListEvent) => {
		this.systemReducedMotion = event.matches;
	};

	dispose() {
		if (this.mediaQuery) {
			this.mediaQuery.removeEventListener('change', this.handleSystemMotionChange);
			this.mediaQuery = null;
		}
	}

	get textSelectionEnabled(): boolean {
		return MobileLayoutStore.isMobileLayout() ? false : this.enableTextSelection;
	}

	get useReducedMotion(): boolean {
		return this.syncReducedMotionWithSystem ? this.systemReducedMotion : (this.reducedMotionOverride ?? false);
	}

	get messageGroupSpacingValue(): number {
		return MobileLayoutStore.isMobileLayout() ? 16 : this.messageGroupSpacing;
	}

	get messageGutterValue(): number {
		return MobileLayoutStore.isMobileLayout() ? 12 : this.messageGutter;
	}

	updateSettings(data: Readonly<Partial<AccessibilitySettings>>): void {
		const validated = this.validateSettings(data);

		if (validated.saturationFactor !== undefined) this.saturationFactor = validated.saturationFactor;
		if (validated.alwaysUnderlineLinks !== undefined) this.alwaysUnderlineLinks = validated.alwaysUnderlineLinks;
		if (validated.enableTextSelection !== undefined) this.enableTextSelection = validated.enableTextSelection;
		if (validated.showMessageSendButton !== undefined) this.showMessageSendButton = validated.showMessageSendButton;
		if (validated.showTextareaFocusRing !== undefined) this.showTextareaFocusRing = validated.showTextareaFocusRing;
		if (validated.hideKeyboardHints !== undefined) this.hideKeyboardHints = validated.hideKeyboardHints;
		if (validated.escapeExitsKeyboardMode !== undefined)
			this.escapeExitsKeyboardMode = validated.escapeExitsKeyboardMode;
		if (validated.syncReducedMotionWithSystem !== undefined)
			this.syncReducedMotionWithSystem = validated.syncReducedMotionWithSystem;
		if (validated.reducedMotionOverride !== undefined) this.reducedMotionOverride = validated.reducedMotionOverride;
		if (validated.messageGroupSpacing !== undefined) this.messageGroupSpacing = validated.messageGroupSpacing;
		if (validated.messageGutter !== undefined) this.messageGutter = validated.messageGutter;
		if (validated.fontSize !== undefined) this.fontSize = validated.fontSize;
		if (validated.showUserAvatarsInCompactMode !== undefined)
			this.showUserAvatarsInCompactMode = validated.showUserAvatarsInCompactMode;
		if (validated.mobileStickerAnimationOverridden !== undefined)
			this.mobileStickerAnimationOverridden = validated.mobileStickerAnimationOverridden;
		if (validated.mobileGifAutoPlayOverridden !== undefined)
			this.mobileGifAutoPlayOverridden = validated.mobileGifAutoPlayOverridden;
		if (validated.mobileAnimateEmojiOverridden !== undefined)
			this.mobileAnimateEmojiOverridden = validated.mobileAnimateEmojiOverridden;
		if (validated.mobileStickerAnimationValue !== undefined)
			this.mobileStickerAnimationValue = validated.mobileStickerAnimationValue;
		if (validated.mobileGifAutoPlayValue !== undefined) this.mobileGifAutoPlayValue = validated.mobileGifAutoPlayValue;
		if (validated.mobileAnimateEmojiValue !== undefined)
			this.mobileAnimateEmojiValue = validated.mobileAnimateEmojiValue;
		if (validated.syncThemeAcrossDevices !== undefined) this.syncThemeAcrossDevices = validated.syncThemeAcrossDevices;
		if (validated.localThemeOverride !== undefined) {
			this.localThemeOverride = validated.localThemeOverride;
			persistTheme(this.localThemeOverride);
		}
		if (validated.showMessageDividers !== undefined) this.showMessageDividers = validated.showMessageDividers;
		if (validated.autoSendTenorGifs !== undefined) this.autoSendTenorGifs = validated.autoSendTenorGifs;
		if (validated.showGiftButton !== undefined) this.showGiftButton = validated.showGiftButton;
		if (validated.showGifButton !== undefined) this.showGifButton = validated.showGifButton;
		if (validated.showMemesButton !== undefined) this.showMemesButton = validated.showMemesButton;
		if (validated.showStickersButton !== undefined) this.showStickersButton = validated.showStickersButton;
		if (validated.showEmojiButton !== undefined) this.showEmojiButton = validated.showEmojiButton;
		if (validated.showUploadButton !== undefined) this.showUploadButton = validated.showUploadButton;
		if (validated.showMediaFavoriteButton !== undefined)
			this.showMediaFavoriteButton = validated.showMediaFavoriteButton;
		if (validated.showMediaDownloadButton !== undefined)
			this.showMediaDownloadButton = validated.showMediaDownloadButton;
		if (validated.showMediaDeleteButton !== undefined) this.showMediaDeleteButton = validated.showMediaDeleteButton;
		if (validated.showSuppressEmbedsButton !== undefined)
			this.showSuppressEmbedsButton = validated.showSuppressEmbedsButton;
		if (validated.showGifIndicator !== undefined) this.showGifIndicator = validated.showGifIndicator;
		if (validated.showAttachmentExpiryIndicator !== undefined)
			this.showAttachmentExpiryIndicator = validated.showAttachmentExpiryIndicator;
		if (validated.useBrowserLocaleForTimeFormat !== undefined)
			this.useBrowserLocaleForTimeFormat = validated.useBrowserLocaleForTimeFormat;
		if (validated.channelTypingIndicatorMode !== undefined)
			this.channelTypingIndicatorMode = validated.channelTypingIndicatorMode;
		if (validated.showSelectedChannelTypingIndicator !== undefined)
			this.showSelectedChannelTypingIndicator = validated.showSelectedChannelTypingIndicator;
		if (validated.showMessageActionBar !== undefined) this.showMessageActionBar = validated.showMessageActionBar;
		if (validated.showMessageActionBarQuickReactions !== undefined)
			this.showMessageActionBarQuickReactions = validated.showMessageActionBarQuickReactions;
		if (validated.showMessageActionBarShiftExpand !== undefined)
			this.showMessageActionBarShiftExpand = validated.showMessageActionBarShiftExpand;
		if (validated.showMessageActionBarOnlyMoreButton !== undefined)
			this.showMessageActionBarOnlyMoreButton = validated.showMessageActionBarOnlyMoreButton;
		if (validated.showDefaultEmojisInExpressionAutocomplete !== undefined)
			this.showDefaultEmojisInExpressionAutocomplete = validated.showDefaultEmojisInExpressionAutocomplete;
		if (validated.showCustomEmojisInExpressionAutocomplete !== undefined)
			this.showCustomEmojisInExpressionAutocomplete = validated.showCustomEmojisInExpressionAutocomplete;
		if (validated.showStickersInExpressionAutocomplete !== undefined)
			this.showStickersInExpressionAutocomplete = validated.showStickersInExpressionAutocomplete;
		if (validated.showMemesInExpressionAutocomplete !== undefined)
			this.showMemesInExpressionAutocomplete = validated.showMemesInExpressionAutocomplete;
		if (validated.attachmentMediaDimensionSize !== undefined)
			this.attachmentMediaDimensionSize = validated.attachmentMediaDimensionSize;
		if (validated.embedMediaDimensionSize !== undefined)
			this.embedMediaDimensionSize = validated.embedMediaDimensionSize;
		if (validated.voiceChannelJoinRequiresDoubleClick !== undefined)
			this.voiceChannelJoinRequiresDoubleClick = validated.voiceChannelJoinRequiresDoubleClick;
		if (validated.customThemeCss !== undefined) this.customThemeCss = validated.customThemeCss;
		if (validated.showFavorites !== undefined) this.showFavorites = validated.showFavorites;
		if (validated.zoomLevel !== undefined) {
			this.zoomLevel = validated.zoomLevel;
			void this.applyZoom(validated.zoomLevel);
		}
		if (validated.dmMessagePreviewMode !== undefined) this.dmMessagePreviewMode = validated.dmMessagePreviewMode;
		if (validated.enableTTSCommand !== undefined) this.enableTTSCommand = validated.enableTTSCommand;
		if (validated.ttsRate !== undefined) this.ttsRate = validated.ttsRate;
	}

	private validateSettings(data: Readonly<Partial<AccessibilitySettings>>): Partial<AccessibilitySettings> {
		return {
			saturationFactor: Math.max(0, Math.min(1, data.saturationFactor ?? this.saturationFactor)),
			alwaysUnderlineLinks: data.alwaysUnderlineLinks ?? this.alwaysUnderlineLinks,
			enableTextSelection: data.enableTextSelection ?? this.enableTextSelection,
			showMessageSendButton: data.showMessageSendButton ?? this.showMessageSendButton,
			showTextareaFocusRing: data.showTextareaFocusRing ?? this.showTextareaFocusRing,
			hideKeyboardHints: data.hideKeyboardHints ?? this.hideKeyboardHints,
			escapeExitsKeyboardMode: data.escapeExitsKeyboardMode ?? this.escapeExitsKeyboardMode,
			syncReducedMotionWithSystem: data.syncReducedMotionWithSystem ?? this.syncReducedMotionWithSystem,
			reducedMotionOverride: data.reducedMotionOverride ?? this.reducedMotionOverride,
			messageGroupSpacing: data.messageGroupSpacing ?? this.messageGroupSpacing,
			messageGutter: Math.max(0, Math.min(200, data.messageGutter ?? this.messageGutter)),
			fontSize: data.fontSize ?? this.fontSize,
			showUserAvatarsInCompactMode: data.showUserAvatarsInCompactMode ?? this.showUserAvatarsInCompactMode,
			mobileStickerAnimationOverridden: data.mobileStickerAnimationOverridden ?? this.mobileStickerAnimationOverridden,
			mobileGifAutoPlayOverridden: data.mobileGifAutoPlayOverridden ?? this.mobileGifAutoPlayOverridden,
			mobileAnimateEmojiOverridden: data.mobileAnimateEmojiOverridden ?? this.mobileAnimateEmojiOverridden,
			mobileStickerAnimationValue: data.mobileStickerAnimationValue ?? this.mobileStickerAnimationValue,
			mobileGifAutoPlayValue: data.mobileGifAutoPlayValue ?? this.mobileGifAutoPlayValue,
			mobileAnimateEmojiValue: data.mobileAnimateEmojiValue ?? this.mobileAnimateEmojiValue,
			syncThemeAcrossDevices: data.syncThemeAcrossDevices ?? this.syncThemeAcrossDevices,
			localThemeOverride: data.localThemeOverride ?? this.localThemeOverride,
			showMessageDividers: data.showMessageDividers ?? this.showMessageDividers,
			autoSendTenorGifs: data.autoSendTenorGifs ?? this.autoSendTenorGifs,
			showGiftButton: data.showGiftButton ?? this.showGiftButton,
			showGifButton: data.showGifButton ?? this.showGifButton,
			showMemesButton: data.showMemesButton ?? this.showMemesButton,
			showStickersButton: data.showStickersButton ?? this.showStickersButton,
			showEmojiButton: data.showEmojiButton ?? this.showEmojiButton,
			showUploadButton: data.showUploadButton ?? this.showUploadButton,
			showMediaFavoriteButton: data.showMediaFavoriteButton ?? this.showMediaFavoriteButton,
			showMediaDownloadButton: data.showMediaDownloadButton ?? this.showMediaDownloadButton,
			showMediaDeleteButton: data.showMediaDeleteButton ?? this.showMediaDeleteButton,
			showSuppressEmbedsButton: data.showSuppressEmbedsButton ?? this.showSuppressEmbedsButton,
			showGifIndicator: data.showGifIndicator ?? this.showGifIndicator,
			showAttachmentExpiryIndicator:
				typeof data.showAttachmentExpiryIndicator === 'boolean'
					? data.showAttachmentExpiryIndicator
					: this.showAttachmentExpiryIndicator,
			useBrowserLocaleForTimeFormat: data.useBrowserLocaleForTimeFormat ?? this.useBrowserLocaleForTimeFormat,
			channelTypingIndicatorMode: data.channelTypingIndicatorMode ?? this.channelTypingIndicatorMode,
			showSelectedChannelTypingIndicator:
				data.showSelectedChannelTypingIndicator ?? this.showSelectedChannelTypingIndicator,
			showMessageActionBar: data.showMessageActionBar ?? this.showMessageActionBar,
			showMessageActionBarQuickReactions:
				data.showMessageActionBarQuickReactions ?? this.showMessageActionBarQuickReactions,
			showMessageActionBarShiftExpand: data.showMessageActionBarShiftExpand ?? this.showMessageActionBarShiftExpand,
			showMessageActionBarOnlyMoreButton:
				data.showMessageActionBarOnlyMoreButton ?? this.showMessageActionBarOnlyMoreButton,
			showDefaultEmojisInExpressionAutocomplete:
				data.showDefaultEmojisInExpressionAutocomplete ?? this.showDefaultEmojisInExpressionAutocomplete,
			showCustomEmojisInExpressionAutocomplete:
				data.showCustomEmojisInExpressionAutocomplete ?? this.showCustomEmojisInExpressionAutocomplete,
			showStickersInExpressionAutocomplete:
				data.showStickersInExpressionAutocomplete ?? this.showStickersInExpressionAutocomplete,
			showMemesInExpressionAutocomplete:
				data.showMemesInExpressionAutocomplete ?? this.showMemesInExpressionAutocomplete,
			attachmentMediaDimensionSize: data.attachmentMediaDimensionSize ?? this.attachmentMediaDimensionSize,
			embedMediaDimensionSize: data.embedMediaDimensionSize ?? this.embedMediaDimensionSize,
			voiceChannelJoinRequiresDoubleClick:
				data.voiceChannelJoinRequiresDoubleClick ?? this.voiceChannelJoinRequiresDoubleClick,
			customThemeCss: data.customThemeCss !== undefined ? data.customThemeCss : this.customThemeCss,
			showFavorites: data.showFavorites ?? this.showFavorites,
			zoomLevel: Math.max(0.5, Math.min(2.0, data.zoomLevel ?? this.zoomLevel)),
			dmMessagePreviewMode: data.dmMessagePreviewMode ?? this.dmMessagePreviewMode,
			enableTTSCommand: data.enableTTSCommand ?? this.enableTTSCommand,
			ttsRate: Math.max(0.1, Math.min(2.0, data.ttsRate ?? this.ttsRate)),
		};
	}

	subscribe(callback: () => void): () => void {
		return reaction(
			() => ({
				messageGroupSpacing: this.messageGroupSpacing,
				showMessageDividers: this.showMessageDividers,
			}),
			() => callback(),
			{fireImmediately: true},
		);
	}

	async adjustZoom(delta: number): Promise<void> {
		const newZoom = Math.min(2.0, Math.max(0.5, this.zoomLevel + delta));
		this.updateSettings({zoomLevel: newZoom});
	}

	async applyZoom(level: number): Promise<void> {
		const electronApi = (window as {electron?: {setZoomFactor: (factor: number) => void}}).electron;
		if (electronApi) {
			electronApi.setZoomFactor(level);
		} else {
			document.documentElement.style.setProperty('zoom', `${level * 100}%`);
		}
	}

	async applyStoredZoom(): Promise<void> {
		if (this.zoomLevel !== 1.0) {
			await this.applyZoom(this.zoomLevel);
		}
	}
}

export default new AccessibilityStore();
