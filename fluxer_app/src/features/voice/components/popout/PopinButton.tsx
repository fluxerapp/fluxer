// SPDX-License-Identifier: AGPL-3.0-or-later

import {Button} from '@app/features/ui/button/Button';
import styles from '@app/features/voice/components/popout/PoppedOutOverlay.module.css';
import PopoutWindowManager from '@app/features/voice/state/PopoutWindowManager';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowSquareOutIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

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

interface PopinButtonProps {
	message: string;
	popoutKey: string;
	compact?: boolean;
}

export const PopinButton: React.FC<PopinButtonProps> = observer(function PopinButton({
	message,
	popoutKey,
	compact = false,
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
	return (
		<div className={styles.content} data-flx="voice.popout.popin-button.content">
			<ArrowSquareOutIcon
				size={compact ? OVERLAY_ICON_SIZE_COMPACT : OVERLAY_ICON_SIZE}
				weight="bold"
				className={styles.icon}
			/>
			<span className={styles.message}>{message}</span>
			<div className={styles.actions}>
				<Button
					variant="inverted"
					small={compact}
					fitContent
					onClick={handlePopBackIn}
					data-flx="voice.popout.popin-button.pop-back-in"
				>
					{i18n._(POP_BACK_IN_DESCRIPTOR)}
				</Button>
				<Button
					variant="inverted-outline"
					small={compact}
					fitContent
					onClick={handleFocusWindow}
					data-flx="voice.popout.popin-button.focus-window"
				>
					{i18n._(FOCUS_WINDOW_DESCRIPTOR)}
				</Button>
			</div>
		</div>
	);
});
