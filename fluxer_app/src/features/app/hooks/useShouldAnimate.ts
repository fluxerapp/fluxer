// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {useSaveData} from '@app/features/app/hooks/useSaveData';
import UserSettings from '@app/features/user/state/UserSettings';
import {StickerAnimationOptions} from '@fluxer/constants/src/UserConstants';

export type ShouldAnimateKind =
	| 'avatar'
	| 'emoji'
	| 'sticker'
	| 'gif'
	| 'guild_icon'
	| 'banner'
	| 'custom_status_emoji';

export interface UseShouldAnimateOptions {
	kind: ShouldAnimateKind;
	isAnimated?: boolean;
	isHovering?: boolean;
	isFocused?: boolean;
	entitlementOk?: boolean;
}

function isKeptUnderReducedMotion(kind: ShouldAnimateKind): boolean {
	if (kind === 'emoji' || kind === 'gif' || kind === 'sticker') {
		return Accessibility.isAnimationKeptUnderReducedMotion(kind);
	}
	return false;
}

export type AnimationAllowanceMode = 'ALWAYS' | 'ON_INTERACTION' | 'NEVER';

function getKindAllowance(kind: ShouldAnimateKind): AnimationAllowanceMode {
	switch (kind) {
		case 'emoji':
			return UserSettings.getAnimateEmoji() ? 'ALWAYS' : 'ON_INTERACTION';
		case 'gif':
			return UserSettings.getGifAutoPlay() ? 'ALWAYS' : 'ON_INTERACTION';
		case 'sticker': {
			const value = UserSettings.getAnimateStickers();
			if (value === StickerAnimationOptions.ALWAYS_ANIMATE) return 'ALWAYS';
			if (value === StickerAnimationOptions.NEVER_ANIMATE) return 'NEVER';
			return 'ON_INTERACTION';
		}
		case 'avatar':
		case 'guild_icon':
		case 'banner':
		case 'custom_status_emoji':
			return 'ON_INTERACTION';
	}
}

export interface ShouldAnimateDecisionInput {
	isAnimated?: boolean;
	allowance: AnimationAllowanceMode;
	reducedMotion: boolean;
	keptUnderReducedMotion: boolean;
	isInteracting: boolean;
	entitlementOk?: boolean;
	saveData: boolean;
}

export function resolveShouldAnimateDecision({
	isAnimated = true,
	allowance,
	reducedMotion,
	keptUnderReducedMotion,
	isInteracting,
	entitlementOk,
	saveData,
}: ShouldAnimateDecisionInput): boolean {
	if (!isAnimated) return false;
	if (entitlementOk === false) return false;
	if (saveData) return false;
	if (allowance === 'NEVER') return false;
	if (reducedMotion && !keptUnderReducedMotion) return isInteracting;
	if (allowance === 'ALWAYS') return true;
	return isInteracting;
}

export function useShouldAnimate({
	kind,
	isAnimated = true,
	isHovering = false,
	isFocused = false,
	entitlementOk,
}: UseShouldAnimateOptions): boolean {
	const saveData = useSaveData();
	const allowance = getKindAllowance(kind);
	return resolveShouldAnimateDecision({
		isAnimated,
		allowance,
		reducedMotion: Accessibility.useReducedMotion,
		keptUnderReducedMotion: isKeptUnderReducedMotion(kind),
		isInteracting: isHovering || isFocused,
		entitlementOk,
		saveData,
	});
}
