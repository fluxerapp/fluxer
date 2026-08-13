// SPDX-License-Identifier: AGPL-3.0-or-later

import {WATCH_STREAM_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import styles from '@app/features/voice/components/VoiceParticipantTile.module.css';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {MonitorPlayIcon, PlusIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';

const ADD_STREAM_DESCRIPTOR = msg({
	message: 'Add stream',
	comment: 'Action label on the local participant tile. Starts a new screen share.',
});

interface WatchStreamOverlayProps {
	addStreamTooltipText: string;
	canAddStream: boolean;
	onAddStream: (event: React.SyntheticEvent) => void;
	onWatch: (event?: React.SyntheticEvent) => void;
}

export function WatchStreamOverlay({
	addStreamTooltipText,
	canAddStream,
	onAddStream,
	onWatch,
}: WatchStreamOverlayProps) {
	const {i18n} = useLingui();
	const watchLabel = i18n._(WATCH_STREAM_DESCRIPTOR);
	const addStreamLabel = i18n._(ADD_STREAM_DESCRIPTOR);
	return (
		<div
			className={styles.watchStreamOverlay}
			data-flx="voice.voice-participant-tile.watch-stream-overlay.watch-stream-overlay"
		>
			<div
				className={styles.watchStreamButtons}
				data-flx="voice.voice-participant-tile.watch-stream-overlay.watch-stream-buttons"
			>
				<Button
					variant="primary"
					fitContent
					leftIcon={
						<MonitorPlayIcon
							size={remFromPx(18)}
							weight="fill"
							data-flx="voice.voice-participant-tile.watch-stream-overlay.monitor-play-icon"
						/>
					}
					onClick={onWatch}
					className={clsx(styles.watchStreamButton, styles.watchStreamPrimaryButton)}
					aria-label={watchLabel}
					data-flx="voice.voice-participant-tile.watch-stream-overlay.watch-stream-button"
				>
					<span
						className={styles.watchStreamButtonLabel}
						data-flx="voice.voice-participant-tile.watch-stream-overlay.watch-label"
					>
						{watchLabel}
					</span>
				</Button>
				{canAddStream && (
					<Tooltip text={addStreamTooltipText} data-flx="voice.voice-participant-tile.watch-stream-overlay.tooltip">
						<Button
							variant="secondary"
							fitContent
							leftIcon={
								<PlusIcon
									weight="bold"
									size={remFromPx(18)}
									data-flx="voice.voice-participant-tile.watch-stream-overlay.plus-icon"
								/>
							}
							onClick={onAddStream}
							className={clsx(styles.watchStreamButton, styles.watchStreamSecondaryButton)}
							aria-label={addStreamLabel}
							data-flx="voice.voice-participant-tile.watch-stream-overlay.watch-stream-button.add-stream"
						>
							<span
								className={styles.watchStreamButtonLabel}
								data-flx="voice.voice-participant-tile.watch-stream-overlay.add-label"
							>
								{addStreamLabel}
							</span>
						</Button>
					</Tooltip>
				)}
			</div>
		</div>
	);
}
