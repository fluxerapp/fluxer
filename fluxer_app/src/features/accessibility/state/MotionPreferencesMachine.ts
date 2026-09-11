// SPDX-License-Identifier: AGPL-3.0-or-later

import {StickerAnimationOptions} from '@fluxer/constants/src/UserConstants';
import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export type StickerAnimation = number;
export type AnimatedMediaKind = 'emoji' | 'gif' | 'sticker';
export type ReducedMotionSource = 'system' | 'manual';

export const DEFAULT_MOBILE_GIF_AUTO_PLAY = false;
export const DEFAULT_MOBILE_STICKER_ANIMATION: StickerAnimation = StickerAnimationOptions.ANIMATE_ON_INTERACTION;

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

export function selectReducedMotionActive(ctx: MotionPreferencesContext): boolean {
	return ctx.syncWithSystem ? ctx.systemReducedMotion : ctx.manualReducedMotion;
}

export function selectReducedMotionSource(ctx: MotionPreferencesContext): ReducedMotionSource {
	return ctx.syncWithSystem ? 'system' : 'manual';
}

export function selectSmoothScrollingEnabled(ctx: MotionPreferencesContext): boolean {
	return !selectReducedMotionActive(ctx);
}

export function selectBaseAnimateEmoji(ctx: MotionPreferencesContext): boolean {
	if (ctx.isMobile && ctx.mobileAnimateEmojiOverridden) {
		return ctx.mobileAnimateEmojiValue;
	}
	return ctx.animateEmoji;
}

export function selectBaseGifAutoPlay(ctx: MotionPreferencesContext): boolean {
	if (ctx.isMobile) {
		return ctx.mobileGifAutoPlayOverridden ? ctx.mobileGifAutoPlayValue : DEFAULT_MOBILE_GIF_AUTO_PLAY;
	}
	return ctx.gifAutoPlay;
}

export function selectBaseAnimateStickers(ctx: MotionPreferencesContext): StickerAnimation {
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

export function selectIsAnimationKeptUnderReducedMotion(
	ctx: MotionPreferencesContext,
	kind: AnimatedMediaKind,
): boolean {
	switch (kind) {
		case 'emoji':
			return ctx.keepAnimatedEmojiUnderReducedMotion;
		case 'gif':
			return ctx.keepGifAutoPlayUnderReducedMotion;
		case 'sticker':
			return ctx.keepStickerAnimationUnderReducedMotion;
	}
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

export function selectMotionPreferencesModel(ctx: MotionPreferencesContext): MotionPreferencesModel {
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

export type MotionPreferencesEvent =
	| {type: 'system.reducedMotionChanged'; value: boolean}
	| {type: 'platform.changed'; isMobile: boolean}
	| {type: 'syncWithSystem.set'; value: boolean}
	| {type: 'manualReducedMotion.set'; value: boolean}
	| {type: 'smoothScrolling.set'; value: boolean}
	| {type: 'basePreferences.synced'; animateEmoji?: boolean; gifAutoPlay?: boolean; animateStickers?: StickerAnimation}
	| {
			type: 'mobileOverrides.synced';
			mobileAnimateEmojiOverridden?: boolean;
			mobileAnimateEmojiValue?: boolean;
			mobileGifAutoPlayOverridden?: boolean;
			mobileGifAutoPlayValue?: boolean;
			mobileStickerAnimationOverridden?: boolean;
			mobileStickerAnimationValue?: StickerAnimation;
	  }
	| {
			type: 'keepFlags.synced';
			keepAnimatedEmojiUnderReducedMotion?: boolean;
			keepGifAutoPlayUnderReducedMotion?: boolean;
			keepStickerAnimationUnderReducedMotion?: boolean;
	  }
	| {type: 'animateEmoji.requested'; value: boolean}
	| {type: 'gifAutoPlay.requested'; value: boolean}
	| {type: 'animateStickers.requested'; value: StickerAnimation};

export const motionPreferencesMachine = setup({
	types: {} as {
		context: MotionPreferencesContext;
		events: MotionPreferencesEvent;
		input: MotionPreferencesInput;
	},
	guards: {
		isReducedMotionActive: ({context}) => selectReducedMotionActive(context),
	},
	actions: {
		applySystemReducedMotion: assign(({context, event}) =>
			event.type === 'system.reducedMotionChanged' ? {systemReducedMotion: event.value} : context,
		),
		applyPlatform: assign(({context, event}) =>
			event.type === 'platform.changed' ? {isMobile: event.isMobile} : context,
		),
		applySyncWithSystem: assign(({context, event}) =>
			event.type === 'syncWithSystem.set' ? {syncWithSystem: event.value} : context,
		),
		applyManualReducedMotion: assign(({context, event}) =>
			event.type === 'manualReducedMotion.set' ? {manualReducedMotion: event.value} : context,
		),
		applySmoothScrolling: assign(({context, event}) =>
			event.type === 'smoothScrolling.set' ? {enableSmoothScrolling: event.value} : context,
		),
		applyBasePreferences: assign(({context, event}) => {
			if (event.type !== 'basePreferences.synced') return context;
			return {
				animateEmoji: event.animateEmoji ?? context.animateEmoji,
				gifAutoPlay: event.gifAutoPlay ?? context.gifAutoPlay,
				animateStickers: event.animateStickers ?? context.animateStickers,
			};
		}),
		applyMobileOverrides: assign(({context, event}) => {
			if (event.type !== 'mobileOverrides.synced') return context;
			return {
				mobileAnimateEmojiOverridden: event.mobileAnimateEmojiOverridden ?? context.mobileAnimateEmojiOverridden,
				mobileAnimateEmojiValue: event.mobileAnimateEmojiValue ?? context.mobileAnimateEmojiValue,
				mobileGifAutoPlayOverridden: event.mobileGifAutoPlayOverridden ?? context.mobileGifAutoPlayOverridden,
				mobileGifAutoPlayValue: event.mobileGifAutoPlayValue ?? context.mobileGifAutoPlayValue,
				mobileStickerAnimationOverridden:
					event.mobileStickerAnimationOverridden ?? context.mobileStickerAnimationOverridden,
				mobileStickerAnimationValue: event.mobileStickerAnimationValue ?? context.mobileStickerAnimationValue,
			};
		}),
		applyKeepFlags: assign(({context, event}) => {
			if (event.type !== 'keepFlags.synced') return context;
			return {
				keepAnimatedEmojiUnderReducedMotion:
					event.keepAnimatedEmojiUnderReducedMotion ?? context.keepAnimatedEmojiUnderReducedMotion,
				keepGifAutoPlayUnderReducedMotion:
					event.keepGifAutoPlayUnderReducedMotion ?? context.keepGifAutoPlayUnderReducedMotion,
				keepStickerAnimationUnderReducedMotion:
					event.keepStickerAnimationUnderReducedMotion ?? context.keepStickerAnimationUnderReducedMotion,
			};
		}),
		applyAnimateEmojiRequest: assign(({context, event}) =>
			event.type === 'animateEmoji.requested' ? resolveAnimateEmojiRequest(context, event.value) : context,
		),
		applyGifAutoPlayRequest: assign(({context, event}) =>
			event.type === 'gifAutoPlay.requested' ? resolveGifAutoPlayRequest(context, event.value) : context,
		),
		applyAnimateStickersRequest: assign(({context, event}) =>
			event.type === 'animateStickers.requested' ? resolveAnimateStickersRequest(context, event.value) : context,
		),
	},
}).createMachine({
	id: 'motionPreferences',
	context: ({input}) => createMotionPreferencesContext(input),
	initial: 'evaluating',
	states: {
		evaluating: {
			always: [{guard: 'isReducedMotionActive', target: 'reduced'}, {target: 'full'}],
		},
		full: {
			on: {
				'system.reducedMotionChanged': {actions: 'applySystemReducedMotion', target: 'evaluating'},
				'platform.changed': {actions: 'applyPlatform', target: 'evaluating'},
				'syncWithSystem.set': {actions: 'applySyncWithSystem', target: 'evaluating'},
				'manualReducedMotion.set': {actions: 'applyManualReducedMotion', target: 'evaluating'},
				'smoothScrolling.set': {actions: 'applySmoothScrolling', target: 'evaluating'},
				'basePreferences.synced': {actions: 'applyBasePreferences', target: 'evaluating'},
				'mobileOverrides.synced': {actions: 'applyMobileOverrides', target: 'evaluating'},
				'keepFlags.synced': {actions: 'applyKeepFlags', target: 'evaluating'},
				'animateEmoji.requested': {actions: 'applyAnimateEmojiRequest', target: 'evaluating'},
				'gifAutoPlay.requested': {actions: 'applyGifAutoPlayRequest', target: 'evaluating'},
				'animateStickers.requested': {actions: 'applyAnimateStickersRequest', target: 'evaluating'},
			},
		},
		reduced: {
			on: {
				'system.reducedMotionChanged': {actions: 'applySystemReducedMotion', target: 'evaluating'},
				'platform.changed': {actions: 'applyPlatform', target: 'evaluating'},
				'syncWithSystem.set': {actions: 'applySyncWithSystem', target: 'evaluating'},
				'manualReducedMotion.set': {actions: 'applyManualReducedMotion', target: 'evaluating'},
				'smoothScrolling.set': {actions: 'applySmoothScrolling', target: 'evaluating'},
				'basePreferences.synced': {actions: 'applyBasePreferences', target: 'evaluating'},
				'mobileOverrides.synced': {actions: 'applyMobileOverrides', target: 'evaluating'},
				'keepFlags.synced': {actions: 'applyKeepFlags', target: 'evaluating'},
				'animateEmoji.requested': {actions: 'applyAnimateEmojiRequest', target: 'evaluating'},
				'gifAutoPlay.requested': {actions: 'applyGifAutoPlayRequest', target: 'evaluating'},
				'animateStickers.requested': {actions: 'applyAnimateStickersRequest', target: 'evaluating'},
			},
		},
	},
});

export type MotionPreferencesSnapshot = SnapshotFrom<typeof motionPreferencesMachine>;
export type MotionPreferencesStateValue = 'full' | 'reduced';

export function createMotionPreferencesSnapshot(input: MotionPreferencesInput = {}): MotionPreferencesSnapshot {
	return initialTransition(motionPreferencesMachine, input)[0];
}

export function transitionMotionPreferencesSnapshot(
	snapshot: MotionPreferencesSnapshot,
	event: MotionPreferencesEvent,
): MotionPreferencesSnapshot {
	return transition(motionPreferencesMachine, snapshot, event)[0] as MotionPreferencesSnapshot;
}

export function getMotionPreferencesStateValue(snapshot: MotionPreferencesSnapshot): MotionPreferencesStateValue {
	return snapshot.value === 'reduced' ? 'reduced' : 'full';
}

export function resolveMotionPreferencesModel(input: MotionPreferencesInput): MotionPreferencesModel {
	return selectMotionPreferencesModel(createMotionPreferencesContext(input));
}
