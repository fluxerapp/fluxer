// SPDX-License-Identifier: AGPL-3.0-or-later

import {StickerAnimationOptions} from '@fluxer/constants/src/UserConstants';
export type StickerAnimation = number;
export type AnimatedMediaKind = 'emoji' | 'gif' | 'sticker';
type ReducedMotionSource = 'system' | 'manual';

const DEFAULT_MOBILE_GIF_AUTO_PLAY = false;
const DEFAULT_MOBILE_STICKER_ANIMATION: StickerAnimation = StickerAnimationOptions.ANIMATE_ON_INTERACTION;

export interface MotionPreferencesInput {
	syncWithSystem?: boolean;
	manualReducedMotion?: boolean;
	systemReducedMotion?: boolean;
	enableSmoothScrolling?: boolean;
	isMobile?: boolean;
	animateEmoji?: boolean;
	gifAutoPlay?: boolean;
	animateStickers?: StickerAnimation;
	mobileAnimateEmojiOverridden?: boolean;
	mobileAnimateEmojiValue?: boolean;
	mobileGifAutoPlayOverridden?: boolean;
	mobileGifAutoPlayValue?: boolean;
	mobileStickerAnimationOverridden?: boolean;
	mobileStickerAnimationValue?: StickerAnimation;
	keepAnimatedEmojiUnderReducedMotion?: boolean;
	keepGifAutoPlayUnderReducedMotion?: boolean;
	keepStickerAnimationUnderReducedMotion?: boolean;
}

export interface MotionPreferencesContext {
	syncWithSystem: boolean;
	manualReducedMotion: boolean;
	systemReducedMotion: boolean;
	enableSmoothScrolling: boolean;
	isMobile: boolean;
	animateEmoji: boolean;
	gifAutoPlay: boolean;
	animateStickers: StickerAnimation;
	mobileAnimateEmojiOverridden: boolean;
	mobileAnimateEmojiValue: boolean;
	mobileGifAutoPlayOverridden: boolean;
	mobileGifAutoPlayValue: boolean;
	mobileStickerAnimationOverridden: boolean;
	mobileStickerAnimationValue: StickerAnimation;
	keepAnimatedEmojiUnderReducedMotion: boolean;
	keepGifAutoPlayUnderReducedMotion: boolean;
	keepStickerAnimationUnderReducedMotion: boolean;
}

export interface MotionPreferencesWrite {
	animateEmoji?: boolean;
	gifAutoPlay?: boolean;
	animateStickers?: StickerAnimation;
	mobileAnimateEmojiOverridden?: boolean;
	mobileAnimateEmojiValue?: boolean;
	mobileGifAutoPlayOverridden?: boolean;
	mobileGifAutoPlayValue?: boolean;
	mobileStickerAnimationOverridden?: boolean;
	mobileStickerAnimationValue?: StickerAnimation;
	keepAnimatedEmojiUnderReducedMotion?: boolean;
	keepGifAutoPlayUnderReducedMotion?: boolean;
	keepStickerAnimationUnderReducedMotion?: boolean;
}

export interface MotionPreferencesModel {
	reducedMotion: boolean;
	reducedMotionSource: ReducedMotionSource;
	smoothScrolling: boolean;
	effectiveAnimateEmoji: boolean;
	effectiveGifAutoPlay: boolean;
	effectiveAnimateStickers: StickerAnimation;
	emojiOverridesReducedMotion: boolean;
	gifOverridesReducedMotion: boolean;
	stickerOverridesReducedMotion: boolean;
}

export function createMotionPreferencesContext(input: MotionPreferencesInput = {}): MotionPreferencesContext {
	return {
		syncWithSystem: input.syncWithSystem ?? true,
		manualReducedMotion: input.manualReducedMotion ?? false,
		systemReducedMotion: input.systemReducedMotion ?? false,
		enableSmoothScrolling: input.enableSmoothScrolling ?? true,
		isMobile: input.isMobile ?? false,
		animateEmoji: input.animateEmoji ?? true,
		gifAutoPlay: input.gifAutoPlay ?? true,
		animateStickers: input.animateStickers ?? StickerAnimationOptions.ALWAYS_ANIMATE,
		mobileAnimateEmojiOverridden: input.mobileAnimateEmojiOverridden ?? false,
		mobileAnimateEmojiValue: input.mobileAnimateEmojiValue ?? true,
		mobileGifAutoPlayOverridden: input.mobileGifAutoPlayOverridden ?? false,
		mobileGifAutoPlayValue: input.mobileGifAutoPlayValue ?? DEFAULT_MOBILE_GIF_AUTO_PLAY,
		mobileStickerAnimationOverridden: input.mobileStickerAnimationOverridden ?? false,
		mobileStickerAnimationValue: input.mobileStickerAnimationValue ?? DEFAULT_MOBILE_STICKER_ANIMATION,
		keepAnimatedEmojiUnderReducedMotion: input.keepAnimatedEmojiUnderReducedMotion ?? false,
		keepGifAutoPlayUnderReducedMotion: input.keepGifAutoPlayUnderReducedMotion ?? false,
		keepStickerAnimationUnderReducedMotion: input.keepStickerAnimationUnderReducedMotion ?? false,
	};
}

function selectReducedMotionActive(ctx: MotionPreferencesContext): boolean {
	return ctx.syncWithSystem ? ctx.systemReducedMotion : ctx.manualReducedMotion;
}

function selectReducedMotionSource(ctx: MotionPreferencesContext): ReducedMotionSource {
	return ctx.syncWithSystem ? 'system' : 'manual';
}

function selectSmoothScrollingEnabled(ctx: MotionPreferencesContext): boolean {
	return !selectReducedMotionActive(ctx);
}

function selectBaseAnimateEmoji(ctx: MotionPreferencesContext): boolean {
	if (ctx.isMobile && ctx.mobileAnimateEmojiOverridden) {
		return ctx.mobileAnimateEmojiValue;
	}
	return ctx.animateEmoji;
}

function selectBaseGifAutoPlay(ctx: MotionPreferencesContext): boolean {
	if (ctx.isMobile) {
		return ctx.mobileGifAutoPlayOverridden ? ctx.mobileGifAutoPlayValue : DEFAULT_MOBILE_GIF_AUTO_PLAY;
	}
	return ctx.gifAutoPlay;
}

function selectBaseAnimateStickers(ctx: MotionPreferencesContext): StickerAnimation {
	if (ctx.isMobile) {
		return ctx.mobileStickerAnimationOverridden ? ctx.mobileStickerAnimationValue : DEFAULT_MOBILE_STICKER_ANIMATION;
	}
	return ctx.animateStickers;
}

function downgradeStickerForReducedMotion(value: StickerAnimation): StickerAnimation {
	return value === StickerAnimationOptions.ALWAYS_ANIMATE ? StickerAnimationOptions.ANIMATE_ON_INTERACTION : value;
}

export function selectEffectiveAnimateEmoji(ctx: MotionPreferencesContext): boolean {
	const base = selectBaseAnimateEmoji(ctx);
	if (!selectReducedMotionActive(ctx)) {
		return base;
	}
	return ctx.keepAnimatedEmojiUnderReducedMotion ? base : false;
}

export function selectEffectiveGifAutoPlay(ctx: MotionPreferencesContext): boolean {
	const base = selectBaseGifAutoPlay(ctx);
	if (!selectReducedMotionActive(ctx)) {
		return base;
	}
	return ctx.keepGifAutoPlayUnderReducedMotion ? base : false;
}

export function selectEffectiveAnimateStickers(ctx: MotionPreferencesContext): StickerAnimation {
	const base = selectBaseAnimateStickers(ctx);
	if (!selectReducedMotionActive(ctx)) {
		return base;
	}
	return ctx.keepStickerAnimationUnderReducedMotion ? base : downgradeStickerForReducedMotion(base);
}

function selectEmojiOverridesReducedMotion(ctx: MotionPreferencesContext): boolean {
	return selectReducedMotionActive(ctx) && ctx.keepAnimatedEmojiUnderReducedMotion && selectBaseAnimateEmoji(ctx);
}

function selectGifOverridesReducedMotion(ctx: MotionPreferencesContext): boolean {
	return selectReducedMotionActive(ctx) && ctx.keepGifAutoPlayUnderReducedMotion && selectBaseGifAutoPlay(ctx);
}

function selectStickerOverridesReducedMotion(ctx: MotionPreferencesContext): boolean {
	return (
		selectReducedMotionActive(ctx) &&
		ctx.keepStickerAnimationUnderReducedMotion &&
		selectBaseAnimateStickers(ctx) === StickerAnimationOptions.ALWAYS_ANIMATE
	);
}

function selectMotionPreferencesModel(ctx: MotionPreferencesContext): MotionPreferencesModel {
	return {
		reducedMotion: selectReducedMotionActive(ctx),
		reducedMotionSource: selectReducedMotionSource(ctx),
		smoothScrolling: selectSmoothScrollingEnabled(ctx),
		effectiveAnimateEmoji: selectEffectiveAnimateEmoji(ctx),
		effectiveGifAutoPlay: selectEffectiveGifAutoPlay(ctx),
		effectiveAnimateStickers: selectEffectiveAnimateStickers(ctx),
		emojiOverridesReducedMotion: selectEmojiOverridesReducedMotion(ctx),
		gifOverridesReducedMotion: selectGifOverridesReducedMotion(ctx),
		stickerOverridesReducedMotion: selectStickerOverridesReducedMotion(ctx),
	};
}

function setBaseEmoji(ctx: MotionPreferencesContext, value: boolean): MotionPreferencesWrite {
	return ctx.isMobile ? {mobileAnimateEmojiOverridden: true, mobileAnimateEmojiValue: value} : {animateEmoji: value};
}

function setBaseGif(ctx: MotionPreferencesContext, value: boolean): MotionPreferencesWrite {
	return ctx.isMobile ? {mobileGifAutoPlayOverridden: true, mobileGifAutoPlayValue: value} : {gifAutoPlay: value};
}

function setBaseStickers(ctx: MotionPreferencesContext, value: StickerAnimation): MotionPreferencesWrite {
	return ctx.isMobile
		? {mobileStickerAnimationOverridden: true, mobileStickerAnimationValue: value}
		: {animateStickers: value};
}

export function resolveAnimateEmojiRequest(ctx: MotionPreferencesContext, value: boolean): MotionPreferencesWrite {
	if (!selectReducedMotionActive(ctx)) {
		return setBaseEmoji(ctx, value);
	}
	if (value) {
		return {...setBaseEmoji(ctx, true), keepAnimatedEmojiUnderReducedMotion: true};
	}
	return {keepAnimatedEmojiUnderReducedMotion: false};
}

export function resolveGifAutoPlayRequest(ctx: MotionPreferencesContext, value: boolean): MotionPreferencesWrite {
	if (!selectReducedMotionActive(ctx)) {
		return setBaseGif(ctx, value);
	}
	if (value) {
		return {...setBaseGif(ctx, true), keepGifAutoPlayUnderReducedMotion: true};
	}
	return {keepGifAutoPlayUnderReducedMotion: false};
}

export function resolveAnimateStickersRequest(
	ctx: MotionPreferencesContext,
	value: StickerAnimation,
): MotionPreferencesWrite {
	if (!selectReducedMotionActive(ctx)) {
		return setBaseStickers(ctx, value);
	}
	if (value === StickerAnimationOptions.ALWAYS_ANIMATE) {
		return {...setBaseStickers(ctx, value), keepStickerAnimationUnderReducedMotion: true};
	}
	return {...setBaseStickers(ctx, value), keepStickerAnimationUnderReducedMotion: false};
}

export function resolveMotionPreferencesModel(input: MotionPreferencesInput): MotionPreferencesModel {
	return selectMotionPreferencesModel(createMotionPreferencesContext(input));
}
