// SPDX-License-Identifier: AGPL-3.0-or-later

import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import styles from '@app/features/voice/components/PiPOverlay.module.css';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowLeftIcon} from '@phosphor-icons/react';
import type React from 'react';

const BACK_TO_CALL_DESCRIPTOR = msg({
	message: 'Back to call',
	comment: 'Button label on the PiP overlay. Returns to the full call view.',
});
interface PiPHeaderProps {
	channelName: string;
	onReturnToCall: (event: React.MouseEvent<HTMLButtonElement>) => void;
}

export function PiPHeader({channelName, onReturnToCall}: PiPHeaderProps) {
	const {i18n} = useLingui();
	return (
		<div className={styles.headerGradient} data-flx="voice.pi-p-overlay.pi-p-overlay-inner.header-gradient">
			<div className={styles.headerContent} data-flx="voice.pi-p-overlay.pi-p-overlay-inner.header-content">
				<div className={styles.headerLeft} data-flx="voice.pi-p-overlay.pi-p-overlay-inner.header-left">
					<FocusRing offset={-2} data-flx="voice.pi-p-overlay.pi-p-overlay-inner.focus-ring">
						<button
							type="button"
							className={styles.returnToCallButton}
							onClick={onReturnToCall}
							onPointerDown={(e) => e.stopPropagation()}
							aria-label={i18n._(BACK_TO_CALL_DESCRIPTOR)}
							data-flx="voice.pi-p-overlay.pi-p-overlay-inner.return-to-call-button"
						>
							<ArrowLeftIcon
								weight="bold"
								className={styles.returnToCallIcon}
								data-flx="voice.pi-p-overlay.pi-p-overlay-inner.return-to-call-icon"
							/>
							<span
								className={styles.returnToCallLabel}
								data-flx="voice.pi-p-overlay.pi-p-overlay-inner.return-to-call-label"
							>
								{channelName}
							</span>
						</button>
					</FocusRing>
				</div>
			</div>
		</div>
	);
}
