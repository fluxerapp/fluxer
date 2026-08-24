// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ADD_TO_FAVORITES_DESCRIPTOR,
	COPY_LINK_DESCRIPTOR,
	REMOVE_FROM_FAVORITES_DESCRIPTOR,
	ZOOM_IN_DESCRIPTOR,
	ZOOM_OUT_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import styles from '@app/features/messaging/components/modals/MediaModal.module.css';
import {
	CLOSE_MODAL_DESCRIPTOR,
	COPY_MEDIA_TO_CLIPBOARD_DESCRIPTOR,
	DOWNLOAD_MEDIA_DESCRIPTOR,
	FORWARD_MEDIA_DESCRIPTOR,
	MEDIA_CONTROLS_DESCRIPTOR,
	OPEN_IN_BROWSER_DESCRIPTOR,
	REPLY_TO_MEDIA_MESSAGE_DESCRIPTOR,
	RESET_MEDIA_POSITION_DESCRIPTOR,
	ROTATE_ANTICLOCKWISE_DESCRIPTOR,
	ROTATE_CLOCKWISE_DESCRIPTOR,
} from '@app/features/messaging/components/modals/media_modal/shared';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {useLingui} from '@lingui/react/macro';
import {
	ArrowBendUpLeftIcon,
	ArrowBendUpRightIcon,
	ArrowClockwiseIcon,
	ArrowCounterClockwiseIcon,
	ArrowSquareOutIcon,
	ArrowsInSimpleIcon,
	CopySimpleIcon,
	DownloadSimpleIcon,
	LinkIcon,
	MagnifyingGlassMinusIcon,
	MagnifyingGlassPlusIcon,
	StarIcon,
	XIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {type FC, forwardRef, type ReactNode, type Ref} from 'react';

interface ControlButtonProps {
	icon: ReactNode;
	label: string;
	onClick: () => void;
	variant?: 'default' | 'primary' | 'danger';
	active?: boolean;
	disabled?: boolean;
	className?: string;
}

export const ControlButton = observer(
	forwardRef<HTMLButtonElement, ControlButtonProps>(
		({icon, label, onClick, variant = 'default', active = false, disabled = false, className}, ref) => {
			const getVariantClass = () => {
				if (active) {
					if (variant === 'primary') return styles.controlButtonPrimaryActive;
					if (variant === 'danger') return styles.controlButtonDangerActive;
					return styles.controlButtonDefaultActive;
				}
				if (variant === 'primary') return styles.controlButtonPrimary;
				if (variant === 'danger') return styles.controlButtonDanger;
				return styles.controlButtonDefault;
			};
			return (
				<FocusRing offset={-2} enabled={!disabled} data-flx="messaging.media-modal.focus-ring">
					<button
						ref={ref}
						type="button"
						onClick={disabled ? undefined : onClick}
						className={clsx(
							styles.controlButton,
							getVariantClass(),
							disabled && styles.controlButtonDisabled,
							className,
						)}
						aria-label={label}
						aria-pressed={active || undefined}
						disabled={disabled}
						data-flx="messaging.media-modal.control-button"
					>
						{icon}
					</button>
				</FocusRing>
			);
		},
	),
);

ControlButton.displayName = 'ControlButton';

interface OverlayTooltipButtonProps extends ControlButtonProps {
	tooltipPosition?: 'top' | 'bottom' | 'left' | 'right';
}

const OverlayTooltipButton: FC<OverlayTooltipButtonProps> = ({
	label,
	tooltipPosition = 'top',
	...buttonProps
}: OverlayTooltipButtonProps) => (
	<Tooltip text={label} position={tooltipPosition} data-flx="messaging.media-modal.overlay-tooltip-button.tooltip">
		<span className={styles.overlayActionButtonWrap} data-flx="messaging.media-modal.overlay-tooltip-button.wrap">
			<ControlButton
				label={label}
				data-flx="messaging.media-modal.media-controls.overlay-tooltip-button.control-button"
				{...buttonProps}
			/>
		</span>
	</Tooltip>
);

interface MediaOverlayActionsProps {
	isFavorited?: boolean;
	onFavorite?: () => void;
	onDownload?: () => void;
	onOpenInBrowser?: () => void;
	onCopyLink?: () => void;
	onCopyMedia?: () => void;
	onReset?: () => void;
	onZoomIn?: () => void;
	onZoomOut?: () => void;
	onRotateCW?: () => void;
	onRotateCCW?: () => void;
	onReply?: () => void;
	onForward?: () => void;
	onClose: () => void;
	canReset?: boolean;
	canZoomIn?: boolean;
	canZoomOut?: boolean;
	enableZoomControls?: boolean;
	onPointerEnter?: () => void;
	onPointerLeave?: () => void;
	rootRef?: Ref<HTMLDivElement>;
}

export const MediaOverlayActions: FC<MediaOverlayActionsProps> = observer(
	({
		isFavorited,
		onFavorite,
		onDownload,
		onOpenInBrowser,
		onCopyLink,
		onCopyMedia,
		onReset,
		onZoomIn,
		onZoomOut,
		onRotateCW,
		onRotateCCW,
		onReply,
		onForward,
		onClose,
		canReset = false,
		canZoomIn = true,
		canZoomOut = true,
		enableZoomControls = false,
		onPointerEnter,
		onPointerLeave,
		rootRef,
	}: MediaOverlayActionsProps) => {
		const {i18n} = useLingui();
		const favoriteLabel = isFavorited ? i18n._(REMOVE_FROM_FAVORITES_DESCRIPTOR) : i18n._(ADD_TO_FAVORITES_DESCRIPTOR);
		return (
			<div
				ref={rootRef}
				className={styles.overlayActionBar}
				role="toolbar"
				aria-label={i18n._(MEDIA_CONTROLS_DESCRIPTOR)}
				onPointerEnter={onPointerEnter}
				onPointerLeave={onPointerLeave}
				data-flx="messaging.media-modal.media-overlay-actions.action-bar"
			>
				<OverlayTooltipButton
					icon={<XIcon size={20} weight="bold" data-flx="messaging.media-modal.media-overlay-actions.close-icon" />}
					label={i18n._(CLOSE_MODAL_DESCRIPTOR)}
					onClick={onClose}
					variant="danger"
					data-flx="messaging.media-modal.media-controls.media-overlay-actions.overlay-tooltip-button.close"
				/>
				<div
					className={styles.overlayActionGap}
					aria-hidden="true"
					data-flx="messaging.media-modal.media-controls.media-overlay-actions.overlay-action-gap"
				/>
				{onOpenInBrowser && (
					<OverlayTooltipButton
						icon={
							<ArrowSquareOutIcon
								size={20}
								weight="bold"
								data-flx="messaging.media-modal.media-overlay-actions.open-icon"
							/>
						}
						label={i18n._(OPEN_IN_BROWSER_DESCRIPTOR)}
						onClick={onOpenInBrowser}
						data-flx="messaging.media-modal.media-controls.media-overlay-actions.overlay-tooltip-button.open-in-browser"
					/>
				)}
				{onCopyLink && (
					<OverlayTooltipButton
						icon={<LinkIcon size={20} weight="bold" data-flx="messaging.media-modal.media-overlay-actions.link-icon" />}
						label={i18n._(COPY_LINK_DESCRIPTOR)}
						onClick={onCopyLink}
						data-flx="messaging.media-modal.media-controls.media-overlay-actions.overlay-tooltip-button.copy-link"
					/>
				)}
				{onCopyMedia && (
					<OverlayTooltipButton
						icon={
							<CopySimpleIcon
								size={20}
								weight="bold"
								data-flx="messaging.media-modal.media-overlay-actions.copy-icon"
							/>
						}
						label={i18n._(COPY_MEDIA_TO_CLIPBOARD_DESCRIPTOR)}
						onClick={onCopyMedia}
						data-flx="messaging.media-modal.media-controls.media-overlay-actions.overlay-tooltip-button.copy-media"
					/>
				)}
				{onFavorite && (
					<OverlayTooltipButton
						icon={
							<StarIcon
								size={20}
								weight={isFavorited ? 'fill' : 'bold'}
								data-flx="messaging.media-modal.media-overlay-actions.favorite-icon"
							/>
						}
						label={favoriteLabel}
						onClick={onFavorite}
						variant={isFavorited ? 'primary' : 'default'}
						active={isFavorited}
						data-flx="messaging.media-modal.media-controls.media-overlay-actions.overlay-tooltip-button.favorite"
					/>
				)}
				{onDownload && (
					<OverlayTooltipButton
						icon={
							<DownloadSimpleIcon
								size={20}
								weight="bold"
								data-flx="messaging.media-modal.media-overlay-actions.download-icon"
							/>
						}
						label={i18n._(DOWNLOAD_MEDIA_DESCRIPTOR)}
						onClick={onDownload}
						data-flx="messaging.media-modal.media-controls.media-overlay-actions.overlay-tooltip-button.download"
					/>
				)}
				<div
					className={styles.overlayActionGap}
					aria-hidden="true"
					data-flx="messaging.media-modal.media-controls.media-overlay-actions.overlay-action-gap--2"
				/>
				{enableZoomControls && (
					<>
						<OverlayTooltipButton
							icon={
								<ArrowCounterClockwiseIcon
									size={20}
									weight="bold"
									data-flx="messaging.media-modal.media-overlay-actions.rotate-ccw-icon"
								/>
							}
							label={i18n._(ROTATE_ANTICLOCKWISE_DESCRIPTOR)}
							onClick={onRotateCCW ?? (() => {})}
							disabled={!onRotateCCW}
							data-flx="messaging.media-modal.media-controls.media-overlay-actions.overlay-tooltip-button"
						/>
						<OverlayTooltipButton
							icon={
								<ArrowClockwiseIcon
									size={20}
									weight="bold"
									data-flx="messaging.media-modal.media-overlay-actions.rotate-cw-icon"
								/>
							}
							label={i18n._(ROTATE_CLOCKWISE_DESCRIPTOR)}
							onClick={onRotateCW ?? (() => {})}
							disabled={!onRotateCW}
							data-flx="messaging.media-modal.media-controls.media-overlay-actions.overlay-tooltip-button--2"
						/>
					</>
				)}
				<OverlayTooltipButton
					icon={
						<ArrowsInSimpleIcon
							size={20}
							weight="bold"
							data-flx="messaging.media-modal.media-overlay-actions.reset-icon"
						/>
					}
					label={i18n._(RESET_MEDIA_POSITION_DESCRIPTOR)}
					onClick={onReset ?? (() => {})}
					disabled={!enableZoomControls || !canReset || !onReset}
					data-flx="messaging.media-modal.media-controls.media-overlay-actions.overlay-tooltip-button--3"
				/>
				<OverlayTooltipButton
					icon={
						<MagnifyingGlassPlusIcon
							size={20}
							weight="bold"
							data-flx="messaging.media-modal.media-overlay-actions.zoom-in-icon"
						/>
					}
					label={i18n._(ZOOM_IN_DESCRIPTOR)}
					onClick={onZoomIn ?? (() => {})}
					disabled={!enableZoomControls || !onZoomIn || !canZoomIn}
					data-flx="messaging.media-modal.media-controls.media-overlay-actions.overlay-tooltip-button--4"
				/>
				<OverlayTooltipButton
					icon={
						<MagnifyingGlassMinusIcon
							size={20}
							weight="bold"
							data-flx="messaging.media-modal.media-overlay-actions.zoom-out-icon"
						/>
					}
					label={i18n._(ZOOM_OUT_DESCRIPTOR)}
					onClick={onZoomOut ?? (() => {})}
					disabled={!enableZoomControls || !onZoomOut || !canZoomOut}
					data-flx="messaging.media-modal.media-controls.media-overlay-actions.overlay-tooltip-button--5"
				/>
				{(onReply || onForward) && (
					<div
						className={styles.overlayActionLargeGap}
						aria-hidden="true"
						data-flx="messaging.media-modal.media-controls.media-overlay-actions.overlay-action-large-gap"
					/>
				)}
				{onReply && (
					<OverlayTooltipButton
						icon={
							<ArrowBendUpLeftIcon
								size={20}
								weight="bold"
								data-flx="messaging.media-modal.media-overlay-actions.reply-icon"
							/>
						}
						label={i18n._(REPLY_TO_MEDIA_MESSAGE_DESCRIPTOR)}
						onClick={onReply}
						data-flx="messaging.media-modal.media-controls.media-overlay-actions.overlay-tooltip-button.reply"
					/>
				)}
				{onForward && (
					<OverlayTooltipButton
						icon={
							<ArrowBendUpRightIcon
								size={20}
								weight="bold"
								data-flx="messaging.media-modal.media-overlay-actions.forward-icon"
							/>
						}
						label={i18n._(FORWARD_MEDIA_DESCRIPTOR)}
						onClick={onForward}
						data-flx="messaging.media-modal.media-controls.media-overlay-actions.overlay-tooltip-button.forward"
					/>
				)}
			</div>
		);
	},
);
