// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createMotionPreferencesContext,
	createMotionPreferencesSnapshot,
	getMotionPreferencesStateValue,
	type MotionPreferencesEvent,
	type MotionPreferencesInput,
	type MotionPreferencesSnapshot,
	resolveAnimateEmojiRequest,
	resolveAnimateStickersRequest,
	resolveGifAutoPlayRequest,
	resolveMotionPreferencesModel,
	selectBaseAnimateStickers,
	selectEffectiveAnimateEmoji,
	selectEffectiveAnimateStickers,
	selectEffectiveGifAutoPlay,
	selectIsAnimationKeptUnderReducedMotion,
	selectReducedMotionActive,
	selectReducedMotionSource,
	selectSmoothScrollingEnabled,
	transitionMotionPreferencesSnapshot,
} from '@app/features/accessibility/state/MotionPreferencesMachine';
import {StickerAnimationOptions} from '@fluxer/constants/src/UserConstants';
import {describe, expect, it} from 'vitest';

const {ALWAYS_ANIMATE, ANIMATE_ON_INTERACTION, NEVER_ANIMATE} = StickerAnimationOptions;

function ctx(input: MotionPreferencesInput = {}) {
	return createMotionPreferencesContext(input);
}

function transition(snapshot: MotionPreferencesSnapshot, event: MotionPreferencesEvent): MotionPreferencesSnapshot {
	return transitionMotionPreferencesSnapshot(snapshot, event);
}

function legacyEffectiveEmoji(base: boolean, reduced: boolean): boolean {
	return reduced ? false : base;
}
function legacyEffectiveGif(base: boolean, reduced: boolean): boolean {
	return reduced ? false : base;
}
function legacyEffectiveSticker(base: number, reduced: boolean): number {
	if (reduced && base === ALWAYS_ANIMATE) return ANIMATE_ON_INTERACTION;
	return base;
}

describe('selectReducedMotionActive / source', () => {
	it('follows the system query when syncing', () => {
		expect(selectReducedMotionActive(ctx({syncWithSystem: true, systemReducedMotion: true}))).toBe(true);
		expect(selectReducedMotionActive(ctx({syncWithSystem: true, systemReducedMotion: false}))).toBe(false);
		expect(selectReducedMotionSource(ctx({syncWithSystem: true}))).toBe('system');
	});

	it('ignores the system query and uses the manual override when not syncing', () => {
		expect(
			selectReducedMotionActive(ctx({syncWithSystem: false, manualReducedMotion: true, systemReducedMotion: false})),
		).toBe(true);
		expect(
			selectReducedMotionActive(ctx({syncWithSystem: false, manualReducedMotion: false, systemReducedMotion: true})),
		).toBe(false);
		expect(selectReducedMotionSource(ctx({syncWithSystem: false}))).toBe('manual');
	});

	it('keeps smooth scrolling tied to the inverse of reduced motion', () => {
		expect(selectSmoothScrollingEnabled(ctx({syncWithSystem: false, manualReducedMotion: false}))).toBe(true);
		expect(selectSmoothScrollingEnabled(ctx({syncWithSystem: false, manualReducedMotion: true}))).toBe(false);
	});
});

describe('effective values without reduced motion', () => {
	it('returns the base desktop preference verbatim', () => {
		const c = ctx({
			syncWithSystem: false,
			manualReducedMotion: false,
			animateEmoji: true,
			gifAutoPlay: false,
			animateStickers: ALWAYS_ANIMATE,
		});
		expect(selectEffectiveAnimateEmoji(c)).toBe(true);
		expect(selectEffectiveGifAutoPlay(c)).toBe(false);
		expect(selectEffectiveAnimateStickers(c)).toBe(ALWAYS_ANIMATE);
	});

	it('applies mobile overrides and defaults', () => {
		const overridden = ctx({
			isMobile: true,
			mobileAnimateEmojiOverridden: true,
			mobileAnimateEmojiValue: false,
			mobileGifAutoPlayOverridden: true,
			mobileGifAutoPlayValue: true,
			mobileStickerAnimationOverridden: true,
			mobileStickerAnimationValue: NEVER_ANIMATE,
		});
		expect(selectEffectiveAnimateEmoji(overridden)).toBe(false);
		expect(selectEffectiveGifAutoPlay(overridden)).toBe(true);
		expect(selectEffectiveAnimateStickers(overridden)).toBe(NEVER_ANIMATE);

		const defaults = ctx({isMobile: true, animateEmoji: true, gifAutoPlay: true, animateStickers: ALWAYS_ANIMATE});
		expect(selectEffectiveAnimateEmoji(defaults)).toBe(true);
		expect(selectEffectiveGifAutoPlay(defaults)).toBe(false);
		expect(selectBaseAnimateStickers(defaults)).toBe(ANIMATE_ON_INTERACTION);
	});
});

describe('reduced motion defaults content animation off (no opt-in)', () => {
	const reduced = {syncWithSystem: false, manualReducedMotion: true} as const;

	it('pauses emoji and gif and downgrades always-animate stickers', () => {
		const c = ctx({...reduced, animateEmoji: true, gifAutoPlay: true, animateStickers: ALWAYS_ANIMATE});
		expect(selectEffectiveAnimateEmoji(c)).toBe(false);
		expect(selectEffectiveGifAutoPlay(c)).toBe(false);
		expect(selectEffectiveAnimateStickers(c)).toBe(ANIMATE_ON_INTERACTION);
	});

	it('leaves on-interaction and never-animate stickers untouched', () => {
		expect(selectEffectiveAnimateStickers(ctx({...reduced, animateStickers: ANIMATE_ON_INTERACTION}))).toBe(
			ANIMATE_ON_INTERACTION,
		);
		expect(selectEffectiveAnimateStickers(ctx({...reduced, animateStickers: NEVER_ANIMATE}))).toBe(NEVER_ANIMATE);
	});
});

describe('reduced motion can be overridden per setting (the fix)', () => {
	const reduced = {syncWithSystem: false, manualReducedMotion: true} as const;

	it('keeps emoji animating when opted in', () => {
		const c = ctx({...reduced, animateEmoji: true, keepAnimatedEmojiUnderReducedMotion: true});
		expect(selectEffectiveAnimateEmoji(c)).toBe(true);
	});

	it('keeps gif autoplay when opted in', () => {
		const c = ctx({...reduced, gifAutoPlay: true, keepGifAutoPlayUnderReducedMotion: true});
		expect(selectEffectiveGifAutoPlay(c)).toBe(true);
	});

	it('keeps always-animate stickers when opted in', () => {
		const c = ctx({...reduced, animateStickers: ALWAYS_ANIMATE, keepStickerAnimationUnderReducedMotion: true});
		expect(selectEffectiveAnimateStickers(c)).toBe(ALWAYS_ANIMATE);
	});

	it('opting in does not resurrect a base preference that is itself off', () => {
		const c = ctx({...reduced, animateEmoji: false, keepAnimatedEmojiUnderReducedMotion: true});
		expect(selectEffectiveAnimateEmoji(c)).toBe(false);
	});

	it('overrides are independent across settings', () => {
		const c = ctx({
			...reduced,
			animateEmoji: true,
			gifAutoPlay: true,
			animateStickers: ALWAYS_ANIMATE,
			keepAnimatedEmojiUnderReducedMotion: true,
		});
		expect(selectEffectiveAnimateEmoji(c)).toBe(true);
		expect(selectEffectiveGifAutoPlay(c)).toBe(false);
		expect(selectEffectiveAnimateStickers(c)).toBe(ANIMATE_ON_INTERACTION);
	});
});

describe('useShouldAnimate gate helper', () => {
	const reduced = {syncWithSystem: false, manualReducedMotion: true} as const;

	it('reports the per-kind keep flag', () => {
		const c = ctx({
			...reduced,
			keepAnimatedEmojiUnderReducedMotion: true,
			keepGifAutoPlayUnderReducedMotion: false,
			keepStickerAnimationUnderReducedMotion: true,
		});
		expect(selectIsAnimationKeptUnderReducedMotion(c, 'emoji')).toBe(true);
		expect(selectIsAnimationKeptUnderReducedMotion(c, 'gif')).toBe(false);
		expect(selectIsAnimationKeptUnderReducedMotion(c, 'sticker')).toBe(true);
	});
});

describe('backwards compatibility with the legacy forced override', () => {
	const bases = [true, false];
	const stickerValues = [ALWAYS_ANIMATE, ANIMATE_ON_INTERACTION, NEVER_ANIMATE];
	for (const reduced of [false, true]) {
		for (const emoji of bases) {
			it(`emoji base=${emoji} reduced=${reduced} matches legacy with no opt-in`, () => {
				const c = ctx({syncWithSystem: false, manualReducedMotion: reduced, animateEmoji: emoji});
				expect(selectEffectiveAnimateEmoji(c)).toBe(legacyEffectiveEmoji(emoji, reduced));
			});
		}
		for (const gif of bases) {
			it(`gif base=${gif} reduced=${reduced} matches legacy with no opt-in`, () => {
				const c = ctx({syncWithSystem: false, manualReducedMotion: reduced, gifAutoPlay: gif});
				expect(selectEffectiveGifAutoPlay(c)).toBe(legacyEffectiveGif(gif, reduced));
			});
		}
		for (const sticker of stickerValues) {
			it(`sticker base=${sticker} reduced=${reduced} matches legacy with no opt-in`, () => {
				const c = ctx({syncWithSystem: false, manualReducedMotion: reduced, animateStickers: sticker});
				expect(selectEffectiveAnimateStickers(c)).toBe(legacyEffectiveSticker(sticker, reduced));
			});
		}
	}
});

describe('resolveAnimateEmojiRequest', () => {
	it('writes the base preference directly when reduced motion is off (desktop)', () => {
		const c = ctx({syncWithSystem: false, manualReducedMotion: false});
		expect(resolveAnimateEmojiRequest(c, true)).toEqual({animateEmoji: true});
		expect(resolveAnimateEmojiRequest(c, false)).toEqual({animateEmoji: false});
	});

	it('writes the mobile override when reduced motion is off (mobile)', () => {
		const c = ctx({isMobile: true, syncWithSystem: false, manualReducedMotion: false});
		expect(resolveAnimateEmojiRequest(c, true)).toEqual({
			mobileAnimateEmojiOverridden: true,
			mobileAnimateEmojiValue: true,
		});
	});

	it('opting in under reduced motion sets the keep flag and the base preference', () => {
		const c = ctx({syncWithSystem: false, manualReducedMotion: true, animateEmoji: false});
		expect(resolveAnimateEmojiRequest(c, true)).toEqual({
			animateEmoji: true,
			keepAnimatedEmojiUnderReducedMotion: true,
		});
	});

	it('opting out under reduced motion only clears the keep flag, preserving the base preference', () => {
		const c = ctx({syncWithSystem: false, manualReducedMotion: true, animateEmoji: true});
		expect(resolveAnimateEmojiRequest(c, false)).toEqual({keepAnimatedEmojiUnderReducedMotion: false});
	});
});

describe('resolveGifAutoPlayRequest', () => {
	it('opting in under reduced motion sets the keep flag', () => {
		const c = ctx({syncWithSystem: false, manualReducedMotion: true});
		expect(resolveGifAutoPlayRequest(c, true)).toEqual({gifAutoPlay: true, keepGifAutoPlayUnderReducedMotion: true});
	});

	it('opting out under reduced motion clears the keep flag', () => {
		const c = ctx({syncWithSystem: false, manualReducedMotion: true});
		expect(resolveGifAutoPlayRequest(c, false)).toEqual({keepGifAutoPlayUnderReducedMotion: false});
	});
});

describe('resolveAnimateStickersRequest', () => {
	it('writes the value directly when reduced motion is off', () => {
		const c = ctx({syncWithSystem: false, manualReducedMotion: false});
		expect(resolveAnimateStickersRequest(c, ALWAYS_ANIMATE)).toEqual({animateStickers: ALWAYS_ANIMATE});
	});

	it('always-animate opts the setting out of reduced motion', () => {
		const c = ctx({syncWithSystem: false, manualReducedMotion: true});
		expect(resolveAnimateStickersRequest(c, ALWAYS_ANIMATE)).toEqual({
			animateStickers: ALWAYS_ANIMATE,
			keepStickerAnimationUnderReducedMotion: true,
		});
	});

	it('reduced-motion-friendly choices store the value and clear the keep flag', () => {
		const c = ctx({syncWithSystem: false, manualReducedMotion: true});
		expect(resolveAnimateStickersRequest(c, ANIMATE_ON_INTERACTION)).toEqual({
			animateStickers: ANIMATE_ON_INTERACTION,
			keepStickerAnimationUnderReducedMotion: false,
		});
		expect(resolveAnimateStickersRequest(c, NEVER_ANIMATE)).toEqual({
			animateStickers: NEVER_ANIMATE,
			keepStickerAnimationUnderReducedMotion: false,
		});
	});

	it('routes through the mobile override on mobile', () => {
		const c = ctx({isMobile: true, syncWithSystem: false, manualReducedMotion: true});
		expect(resolveAnimateStickersRequest(c, ALWAYS_ANIMATE)).toEqual({
			mobileStickerAnimationOverridden: true,
			mobileStickerAnimationValue: ALWAYS_ANIMATE,
			keepStickerAnimationUnderReducedMotion: true,
		});
	});
});

describe('machine state value', () => {
	it('settles to full when reduced motion is inactive', () => {
		const snapshot = createMotionPreferencesSnapshot({syncWithSystem: true, systemReducedMotion: false});
		expect(getMotionPreferencesStateValue(snapshot)).toBe('full');
	});

	it('settles to reduced when reduced motion is active', () => {
		const snapshot = createMotionPreferencesSnapshot({syncWithSystem: false, manualReducedMotion: true});
		expect(getMotionPreferencesStateValue(snapshot)).toBe('reduced');
	});

	it('transitions full -> reduced when the system query flips', () => {
		let snapshot = createMotionPreferencesSnapshot({syncWithSystem: true, systemReducedMotion: false});
		expect(getMotionPreferencesStateValue(snapshot)).toBe('full');
		snapshot = transition(snapshot, {type: 'system.reducedMotionChanged', value: true});
		expect(getMotionPreferencesStateValue(snapshot)).toBe('reduced');
		snapshot = transition(snapshot, {type: 'system.reducedMotionChanged', value: false});
		expect(getMotionPreferencesStateValue(snapshot)).toBe('full');
	});

	it('disabling sync falls back to the manual override and re-routes', () => {
		let snapshot = createMotionPreferencesSnapshot({
			syncWithSystem: true,
			systemReducedMotion: true,
			manualReducedMotion: false,
		});
		expect(getMotionPreferencesStateValue(snapshot)).toBe('reduced');
		snapshot = transition(snapshot, {type: 'syncWithSystem.set', value: false});
		expect(getMotionPreferencesStateValue(snapshot)).toBe('full');
	});
});

describe('machine request lifecycle', () => {
	it('re-enables emoji under reduced motion and restores the base when reduced motion lifts', () => {
		let snapshot = createMotionPreferencesSnapshot({
			syncWithSystem: false,
			manualReducedMotion: true,
			animateEmoji: false,
		});
		expect(selectEffectiveAnimateEmoji(snapshot.context)).toBe(false);

		snapshot = transition(snapshot, {type: 'animateEmoji.requested', value: true});
		expect(snapshot.context.keepAnimatedEmojiUnderReducedMotion).toBe(true);
		expect(snapshot.context.animateEmoji).toBe(true);
		expect(selectEffectiveAnimateEmoji(snapshot.context)).toBe(true);

		snapshot = transition(snapshot, {type: 'manualReducedMotion.set', value: false});
		expect(getMotionPreferencesStateValue(snapshot)).toBe('full');
		expect(selectEffectiveAnimateEmoji(snapshot.context)).toBe(true);
	});

	it('opting an animation back out under reduced motion preserves the pre-reduced-motion base', () => {
		let snapshot = createMotionPreferencesSnapshot({
			syncWithSystem: false,
			manualReducedMotion: false,
			animateEmoji: true,
		});
		snapshot = transition(snapshot, {type: 'manualReducedMotion.set', value: true});
		expect(selectEffectiveAnimateEmoji(snapshot.context)).toBe(false);

		snapshot = transition(snapshot, {type: 'animateEmoji.requested', value: true});
		expect(selectEffectiveAnimateEmoji(snapshot.context)).toBe(true);
		snapshot = transition(snapshot, {type: 'animateEmoji.requested', value: false});
		expect(snapshot.context.keepAnimatedEmojiUnderReducedMotion).toBe(false);
		expect(selectEffectiveAnimateEmoji(snapshot.context)).toBe(false);

		snapshot = transition(snapshot, {type: 'manualReducedMotion.set', value: false});
		expect(selectEffectiveAnimateEmoji(snapshot.context)).toBe(true);
	});

	it('keeps external syncs (base prefs, mobile, keep flags) flowing into the resolved model', () => {
		let snapshot = createMotionPreferencesSnapshot({syncWithSystem: false, manualReducedMotion: true});
		snapshot = transition(snapshot, {type: 'basePreferences.synced', animateStickers: ALWAYS_ANIMATE});
		snapshot = transition(snapshot, {type: 'keepFlags.synced', keepStickerAnimationUnderReducedMotion: true});
		expect(selectEffectiveAnimateStickers(snapshot.context)).toBe(ALWAYS_ANIMATE);
		snapshot = transition(snapshot, {type: 'platform.changed', isMobile: true});
		snapshot = transition(snapshot, {
			type: 'mobileOverrides.synced',
			mobileStickerAnimationOverridden: true,
			mobileStickerAnimationValue: NEVER_ANIMATE,
		});
		expect(selectEffectiveAnimateStickers(snapshot.context)).toBe(NEVER_ANIMATE);
	});
});

describe('resolveMotionPreferencesModel', () => {
	it('flags settings that are overriding reduced motion for the UI', () => {
		const model = resolveMotionPreferencesModel({
			syncWithSystem: false,
			manualReducedMotion: true,
			animateEmoji: true,
			gifAutoPlay: true,
			animateStickers: ALWAYS_ANIMATE,
			keepAnimatedEmojiUnderReducedMotion: true,
			keepStickerAnimationUnderReducedMotion: true,
		});
		expect(model.reducedMotion).toBe(true);
		expect(model.emojiOverridesReducedMotion).toBe(true);
		expect(model.gifOverridesReducedMotion).toBe(false);
		expect(model.stickerOverridesReducedMotion).toBe(true);
		expect(model.effectiveAnimateEmoji).toBe(true);
		expect(model.effectiveGifAutoPlay).toBe(false);
		expect(model.effectiveAnimateStickers).toBe(ALWAYS_ANIMATE);
	});

	it('reports no overrides when reduced motion is off', () => {
		const model = resolveMotionPreferencesModel({
			syncWithSystem: false,
			manualReducedMotion: false,
			keepAnimatedEmojiUnderReducedMotion: true,
		});
		expect(model.reducedMotion).toBe(false);
		expect(model.emojiOverridesReducedMotion).toBe(false);
	});
});
