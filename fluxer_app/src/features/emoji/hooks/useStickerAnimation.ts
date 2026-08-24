// SPDX-License-Identifier: AGPL-3.0-or-later

import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import {useState} from 'react';

interface UseStickerAnimationOptions {
	respectUserSettings?: boolean;
	isInteracting?: boolean;
	isAnimated?: boolean;
}

interface UseStickerAnimationResult {
	shouldAnimate: boolean;
	interactionHandlers: {
		onMouseEnter: () => void;
		onMouseLeave: () => void;
		onFocus: () => void;
		onBlur: () => void;
	};
}

export function useStickerAnimation(options: UseStickerAnimationOptions = {}): UseStickerAnimationResult {
	const {respectUserSettings = true, isInteracting: isInteractingOverride, isAnimated = true} = options;
	const [isInteracting, setIsInteracting] = useState(false);
	const effectiveInteraction = isInteractingOverride ?? isInteracting;
	const shouldAnimate = useShouldAnimate({
		kind: 'sticker',
		isAnimated,
		isHovering: respectUserSettings ? effectiveInteraction : true,
		isFocused: respectUserSettings ? effectiveInteraction : true,
	});
	const interactionHandlers = {
		onMouseEnter: () => setIsInteracting(true),
		onMouseLeave: () => setIsInteracting(false),
		onFocus: () => setIsInteracting(true),
		onBlur: () => setIsInteracting(false),
	};
	return {shouldAnimate, interactionHandlers};
}
