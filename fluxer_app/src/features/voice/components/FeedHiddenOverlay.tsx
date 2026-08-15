// SPDX-License-Identifier: AGPL-3.0-or-later

import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import styles from '@app/features/voice/components/FeedHiddenOverlay.module.css';
import {MonitorPlayIcon} from '@phosphor-icons/react';
import type React from 'react';

interface FeedHiddenOverlayProps {
	message: string;
	buttonLabel: string;
	onReveal: (event: React.SyntheticEvent) => void;
}

export function FeedHiddenOverlay({message, buttonLabel, onReveal}: FeedHiddenOverlayProps) {
	return (
		<div className={styles.feedHiddenOverlay} data-flx="voice.feed-hidden-overlay.feed-hidden-overlay">
			<span className={styles.feedHiddenText} data-flx="voice.feed-hidden-overlay.feed-hidden-text">
				{message}
			</span>
			<Button
				variant="secondary"
				fitContent
				leftIcon={
					<MonitorPlayIcon size={remFromPx(18)} weight="fill" data-flx="voice.feed-hidden-overlay.monitor-play-icon" />
				}
				onClick={onReveal}
				className={styles.feedHiddenButton}
				data-flx="voice.feed-hidden-overlay.feed-hidden-button.reveal"
			>
				{buttonLabel}
			</Button>
		</div>
	);
}
