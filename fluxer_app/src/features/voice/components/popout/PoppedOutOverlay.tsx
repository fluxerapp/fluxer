// SPDX-License-Identifier: AGPL-3.0-or-later

import {Button} from '@app/features/ui/button/Button';
import styles from '@app/features/voice/components/popout/PoppedOutOverlay.module.css';
import type {PoppedOutOverlayTransition} from '@app/features/voice/components/popout/PoppedOutSurfaceStateMachine';
import PopoutWindowManager from '@app/features/voice/state/PopoutWindowManager';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowSquareOutIcon} from '@phosphor-icons/react';
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
const POP_BACK_IN_DESCRIPTOR = msg({
	message: 'Pop back in',
	comment: 'Button label on the popped-out overlay that closes the popout window and restores the view in the app.',
});
const FOCUS_WINDOW_DESCRIPTOR = msg({
	message: 'Focus window',
	comment: 'Button label on the popped-out overlay that brings the popped-out window to the foreground.',
});

const OVERLAY_ICON_SIZE = 32;
const OVERLAY_ICON_SIZE_COMPACT = 22;

type PoppedOutOverlayVariant = 'call' | 'tile';

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
	const handlePopBackIn = useCallback(
		(event: React.MouseEvent<HTMLButtonElement>) => {
			event.stopPropagation();
			PopoutWindowManager.close(popoutKey);
		},
		[popoutKey],
	);
	const handleFocusWindow = useCallback(
		(event: React.MouseEvent<HTMLButtonElement>) => {
			event.stopPropagation();
			PopoutWindowManager.focus(popoutKey);
		},
		[popoutKey],
	);
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
			<div className={styles.content} data-flx="voice.popped-out-overlay.content">
				<ArrowSquareOutIcon
					size={isCompact ? OVERLAY_ICON_SIZE_COMPACT : OVERLAY_ICON_SIZE}
					weight="bold"
					className={styles.icon}
					data-flx="voice.popped-out-overlay.arrow-square-out-icon"
				/>
				<span className={styles.message} data-flx="voice.popped-out-overlay.message">
					{message}
				</span>
				<div className={styles.actions} data-flx="voice.popped-out-overlay.actions">
					<Button
						variant="inverted"
						small={isCompact}
						fitContent
						onClick={handlePopBackIn}
						data-flx="voice.popped-out-overlay.button.pop-back-in"
					>
						{i18n._(POP_BACK_IN_DESCRIPTOR)}
					</Button>
					<Button
						variant="inverted-outline"
						small={isCompact}
						fitContent
						onClick={handleFocusWindow}
						data-flx="voice.popped-out-overlay.button.focus-window"
					>
						{i18n._(FOCUS_WINDOW_DESCRIPTOR)}
					</Button>
				</div>
			</div>
		</div>
	);
});
