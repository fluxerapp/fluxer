// SPDX-License-Identifier: AGPL-3.0-or-later

import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import styles from '@app/features/voice/components/modals/ScreenSharePickerModal.module.css';
import type {PickerCard} from '@app/features/voice/components/modals/screen_share_picker_modal/shared';
import {getDeterministicPlaceholderGradient} from '@app/lib/placeholder-gradient';
import {clsx} from 'clsx';
import {type FC, type Ref, useEffect, useRef} from 'react';

interface DevicePreviewVideoProps {
	stream: MediaStream;
}

const DevicePreviewVideo: FC<DevicePreviewVideoProps> = ({stream}) => {
	const videoRef = useRef<HTMLVideoElement>(null);
	useEffect(() => {
		const video = videoRef.current;
		if (!video) return;
		video.srcObject = stream;
		return () => {
			if (video.srcObject === stream) {
				video.srcObject = null;
			}
		};
	}, [stream]);
	return (
		<video
			ref={videoRef}
			autoPlay
			playsInline
			muted
			className={styles.devicePreviewVideo}
			data-flx="voice.screen-share-picker-modal.device-preview-video"
		/>
	);
};

interface PickerCardButtonProps {
	card: PickerCard;
	isDeviceCard: boolean;
	isPending: boolean;
	isAnyPending: boolean;
	isSelected: boolean;
	devicePreviewStream: MediaStream | null;
	actionLabel: string;
	ariaLabel: string;
	onSelect: () => void;
	onPreviewImageError: () => void;
	buttonRef?: Ref<HTMLButtonElement>;
}

interface PickerCardPreviewProps {
	card: PickerCard;
	isDeviceCard: boolean;
	devicePreviewStream: MediaStream | null;
	actionLabel: string;
	onPreviewImageError: () => void;
}

const PickerCardPreview: FC<PickerCardPreviewProps> = ({
	card,
	isDeviceCard,
	devicePreviewStream,
	actionLabel,
	onPreviewImageError,
}) => {
	const PlaceholderIcon = card.placeholderIcon;
	return (
		<div
			className={clsx(styles.preview, isDeviceCard && styles.previewDevice)}
			data-flx="voice.screen-share-picker-modal.preview"
		>
			{devicePreviewStream ? (
				<DevicePreviewVideo
					stream={devicePreviewStream}
					data-flx="voice.screen-share-picker-modal.picker-card-button.picker-card-preview.device-preview-video"
				/>
			) : card.thumbnailSrc ? (
				<img
					src={card.thumbnailSrc}
					alt=""
					className={styles.previewImage}
					draggable={false}
					onError={onPreviewImageError}
					data-flx="voice.screen-share-picker-modal.preview-image"
				/>
			) : (
				<div
					className={clsx(styles.previewPlaceholder, isDeviceCard && styles.devicePreviewPlaceholder)}
					style={isDeviceCard ? getDeterministicPlaceholderGradient(card.id) : undefined}
					data-flx="voice.screen-share-picker-modal.preview-placeholder"
				>
					<PlaceholderIcon
						className={styles.previewIcon}
						weight="fill"
						data-flx="voice.screen-share-picker-modal.preview-icon"
					/>
				</div>
			)}
			<span
				className={styles.previewOverlay}
				aria-hidden={true}
				data-flx="voice.screen-share-picker-modal.picker-card-button.picker-card-preview.preview-overlay"
			>
				<span
					className={styles.previewAction}
					data-flx="voice.screen-share-picker-modal.picker-card-button.picker-card-preview.preview-action"
				>
					{actionLabel}
				</span>
			</span>
		</div>
	);
};

interface PickerCardBodyProps {
	card: PickerCard;
}

const PickerCardBody: FC<PickerCardBodyProps> = ({card}) => {
	const PlaceholderIcon = card.placeholderIcon;
	return (
		<div className={styles.cardBody} data-flx="voice.screen-share-picker-modal.card-body">
			<span
				className={styles.cardSourceIcon}
				aria-hidden={true}
				data-flx="voice.screen-share-picker-modal.picker-card-button.picker-card-body.card-source-icon"
			>
				{card.badgeSrc ? (
					<img
						src={card.badgeSrc}
						alt=""
						draggable={false}
						className={styles.cardSourceIconImage}
						data-flx="voice.screen-share-picker-modal.card-source-icon-image"
					/>
				) : (
					<PlaceholderIcon weight="fill" data-flx="voice.screen-share-picker-modal.card-source-icon" />
				)}
			</span>
			<div className={styles.cardTitle} data-flx="voice.screen-share-picker-modal.card-title">
				{card.title}
			</div>
		</div>
	);
};

export const PickerCardButton: FC<PickerCardButtonProps> = ({
	card,
	isDeviceCard,
	isPending,
	isAnyPending,
	isSelected,
	devicePreviewStream,
	actionLabel,
	ariaLabel,
	onSelect,
	onPreviewImageError,
	buttonRef,
}) => {
	return (
		<FocusRing key={card.id} offset={-2} data-flx="voice.screen-share-picker-modal.focus-ring">
			<button
				ref={buttonRef}
				type="button"
				className={clsx(styles.card, isPending && styles.cardPending, isSelected && styles.cardSelected)}
				onClick={onSelect}
				disabled={isAnyPending}
				aria-label={ariaLabel}
				aria-pressed={isDeviceCard ? isSelected : undefined}
				data-device-preview-card-id={isDeviceCard ? card.id : undefined}
				data-flx="voice.screen-share-picker-modal.card.button"
			>
				<PickerCardPreview
					card={card}
					isDeviceCard={isDeviceCard}
					devicePreviewStream={devicePreviewStream}
					actionLabel={actionLabel}
					onPreviewImageError={onPreviewImageError}
					data-flx="voice.screen-share-picker-modal.picker-card-button.picker-card-preview"
				/>
				<PickerCardBody card={card} data-flx="voice.screen-share-picker-modal.picker-card-button.picker-card-body" />
			</button>
		</FocusRing>
	);
};
