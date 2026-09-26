// SPDX-License-Identifier: AGPL-3.0-or-later

import {PopinButton} from '@app/features/voice/components/popout/PopinButton';
import styles from '@app/features/voice/components/popout/PoppedOutOverlay.module.css';
import type {PoppedOutOverlayTransition} from '@app/features/voice/components/popout/PoppedOutSurfaceStateMachine';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const CALL_POPPED_OUT_DESCRIPTOR = msg({
	message: 'Call is popped out',
	comment: 'Overlay text shown in the in-app call area while the call view lives in a separate popped-out window.',
});
const TILE_POPPED_OUT_DESCRIPTOR = msg({
	message: 'Popped out',
	comment: 'Overlay text shown on a voice participant tile while it lives in a separate popped-out window.',
});

export type PoppedOutOverlayVariant = 'call' | 'tile';

interface PoppedOutOverlayProps {
	popoutKey: string;
	variant: PoppedOutOverlayVariant;
	transition: PoppedOutOverlayTransition;
	onTransitionEnd: () => void;
	compact?: boolean;
	className?: string;
}

export const PoppedOutOverlay: React.FC<PoppedOutOverlayProps> = observer(function PoppedOutOverlay({
	popoutKey,
	variant,
	transition,
	onTransitionEnd,
	compact = false,
	className,
}) {
	const {i18n} = useLingui();
	const handleAnimationEnd = useCallback(
		(event: React.AnimationEvent<HTMLDivElement>) => {
			if (event.target !== event.currentTarget) return;
			onTransitionEnd();
		},
		[onTransitionEnd],
	);
	const isCompact = compact || variant === 'tile';
	const message = i18n._(variant === 'call' ? CALL_POPPED_OUT_DESCRIPTOR : TILE_POPPED_OUT_DESCRIPTOR);
	return (
		<div
			className={clsx(
				styles.overlay,
				variant === 'call' && styles.overlayCall,
				isCompact && styles.overlayCompact,
				className,
			)}
			data-transition={transition}
			data-voice-popped-out
			onAnimationEnd={handleAnimationEnd}
			role="status"
			data-flx="voice.popped-out-overlay.overlay"
		>
			<PopinButton message={message} popoutKey={popoutKey} compact={isCompact} />
		</div>
	);
});
