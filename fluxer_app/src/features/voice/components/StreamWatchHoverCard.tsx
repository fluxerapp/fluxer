// SPDX-License-Identifier: AGPL-3.0-or-later

import {WATCH_STREAM_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {Spinner} from '@app/features/ui/components/Spinner';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import styles from '@app/features/voice/components/StreamWatchHoverCard.module.css';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {MonitorPlayIcon, PlusIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import {useCallback} from 'react';

const WATCHING_STREAM_DESCRIPTOR = msg({
	message: 'Watching stream',
	comment: 'Status label on the screen-share hover card when the local user is already watching the stream.',
});
const STREAM_PREVIEW_DESCRIPTOR = msg({
	message: 'Stream preview',
	comment: 'Aria label for the small thumbnail preview on the screen-share hover card.',
});
const NO_PREVIEW_YET_DESCRIPTOR = msg({
	message: 'No preview yet',
	comment: 'Placeholder text on the screen-share hover card when no thumbnail has been received yet.',
});
const ADD_STREAM_DESCRIPTOR = msg({
	message: 'Add stream',
	comment: 'Action button on the screen-share hover card. Starts a new local screen share.',
});
const DOUBLE_CLICK_A_STREAMING_USER_IN_THE_PARTICIPANT_DESCRIPTOR = msg({
	message: 'Double-click someone to watch their stream.',
	comment: 'Empty / hint text on the screen-share hover card explaining how to start watching a remote stream.',
});

interface StreamWatchHoverCardProps {
	previewUrl: string | null;
	isPreviewLoading: boolean;
	watchLabel: string;
	addLabel?: string;
	onWatch: (event: React.SyntheticEvent) => void;
	onAddStream?: (event: React.SyntheticEvent) => void;
	onPreviewClick?: (event: React.MouseEvent) => void;
	watchDisabled?: boolean;
	isWatching?: boolean;
	isSubmitting?: boolean;
	variant?: 'compact' | 'list';
	showProtip?: boolean;
	showAddButton?: boolean;
	addTooltip?: string;
}

export const StreamWatchHoverCard: React.FC<StreamWatchHoverCardProps> = ({
	previewUrl,
	isPreviewLoading,
	watchLabel,
	addLabel,
	onWatch,
	onAddStream,
	onPreviewClick,
	watchDisabled = false,
	isWatching = false,
	isSubmitting = false,
	variant = 'list',
	showProtip = false,
	showAddButton = false,
	addTooltip,
}) => {
	const {i18n} = useLingui();
	const isCompact = variant === 'compact';
	const handlePreviewClick = useCallback(
		(event: React.MouseEvent) => {
			event.stopPropagation();
			if (onPreviewClick) {
				onPreviewClick(event);
			} else if (!watchDisabled || isWatching) {
				onWatch(event);
			}
		},
		[onPreviewClick, onWatch, watchDisabled, isWatching],
	);
	const handlePreviewKeyDown = useCallback(
		(event: React.KeyboardEvent) => {
			if (isKeyboardActivationKey(event.key)) {
				event.preventDefault();
				event.stopPropagation();
				if (!watchDisabled || isWatching) {
					onWatch(event);
				}
			}
		},
		[onWatch, watchDisabled, isWatching],
	);
	const previewHoverLabel = isWatching ? i18n._(WATCHING_STREAM_DESCRIPTOR) : i18n._(WATCH_STREAM_DESCRIPTOR);
	return (
		<div
			className={clsx(styles.card, isCompact ? styles.variantCompact : styles.variantList)}
			data-flx="voice.stream-watch-hover-card.card"
		>
			<div
				className={clsx(
					styles.preview,
					isCompact ? styles.previewCompact : styles.previewList,
					!isCompact && styles.previewClickable,
				)}
				data-flx="voice.stream-watch-hover-card.preview"
				{...(!isCompact
					? {
							role: 'button',
							tabIndex: 0,
							onClick: handlePreviewClick,
							onKeyDown: handlePreviewKeyDown,
							'aria-label': previewHoverLabel,
						}
					: {})}
			>
				{previewUrl ? (
					<img
						src={previewUrl}
						alt={i18n._(STREAM_PREVIEW_DESCRIPTOR)}
						className={styles.previewImage}
						data-flx="voice.stream-watch-hover-card.preview-image"
					/>
				) : (
					<div className={styles.previewFallback} data-flx="voice.stream-watch-hover-card.preview-fallback">
						{isPreviewLoading ? (
							<Spinner size="small" data-flx="voice.stream-watch-hover-card.spinner" />
						) : (
							i18n._(NO_PREVIEW_YET_DESCRIPTOR)
						)}
					</div>
				)}
				{!isCompact && (
					<div className={styles.previewHoverOverlay} data-flx="voice.stream-watch-hover-card.preview-hover-overlay">
						<span className={styles.previewHoverText} data-flx="voice.stream-watch-hover-card.preview-hover-text">
							{previewHoverLabel}
						</span>
					</div>
				)}
				{isCompact && (
					<div className={styles.compactButtonWrap} data-flx="voice.stream-watch-hover-card.compact-button-wrap">
						<Button
							fitContent
							leftIcon={
								<MonitorPlayIcon
									size={remFromPx(18)}
									weight="fill"
									data-flx="voice.stream-watch-hover-card.monitor-play-icon"
								/>
							}
							onClick={onWatch}
							disabled={watchDisabled}
							submitting={isSubmitting}
							className={styles.compactButton}
							data-flx="voice.stream-watch-hover-card.compact-button.watch"
						>
							{watchLabel}
						</Button>
					</div>
				)}
			</div>
			{!isCompact && (
				<div className={styles.actionRow} data-flx="voice.stream-watch-hover-card.action-row">
					<div className={styles.actionButtons} data-flx="voice.stream-watch-hover-card.action-buttons">
						<Button
							fitContent
							leftIcon={
								<MonitorPlayIcon
									size={remFromPx(18)}
									weight="fill"
									data-flx="voice.stream-watch-hover-card.monitor-play-icon--2"
								/>
							}
							onClick={onWatch}
							disabled={watchDisabled}
							submitting={isSubmitting}
							className={styles.listButton}
							data-flx="voice.stream-watch-hover-card.list-button.watch"
						>
							{watchLabel}
						</Button>
						{showAddButton && onAddStream && (
							<Tooltip text={addTooltip ?? ''} data-flx="voice.stream-watch-hover-card.tooltip">
								<Button
									fitContent
									leftIcon={<PlusIcon size={remFromPx(18)} data-flx="voice.stream-watch-hover-card.plus-icon" />}
									onClick={onAddStream}
									disabled={watchDisabled}
									className={styles.listButton}
									data-flx="voice.stream-watch-hover-card.list-button.add-stream"
								>
									{addLabel ?? i18n._(ADD_STREAM_DESCRIPTOR)}
								</Button>
							</Tooltip>
						)}
					</div>
					{showProtip && (
						<div className={styles.protipRow} data-flx="voice.stream-watch-hover-card.protip-row">
							<span className={styles.protipText} data-flx="voice.stream-watch-hover-card.protip-text">
								{i18n._(DOUBLE_CLICK_A_STREAMING_USER_IN_THE_PARTICIPANT_DESCRIPTOR)}
							</span>
						</div>
					)}
				</div>
			)}
		</div>
	);
};
