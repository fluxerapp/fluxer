// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ADD_TO_FAVORITES_DESCRIPTOR,
	COPY_LINK_DESCRIPTOR,
	MORE_OPTIONS_DESCRIPTOR,
	REMOVE_FROM_FAVORITES_DESCRIPTOR,
	ZOOM_IN_DESCRIPTOR,
	ZOOM_OUT_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import styles from '@app/features/messaging/components/modals/MediaModal.module.css';
import {ControlButton} from '@app/features/messaging/components/modals/media_modal/MediaControls';
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
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {
	MenuBottomSheet,
	type MenuGroupType,
	type MenuItemType,
} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {useLingui} from '@lingui/react/macro';
import {
	ArrowBendUpLeftIcon,
	ArrowBendUpRightIcon,
	ArrowClockwiseIcon,
	ArrowCounterClockwiseIcon,
	ArrowSquareOutIcon,
	ArrowsInSimpleIcon,
	CopySimpleIcon,
	DotsThreeIcon,
	DownloadSimpleIcon,
	LinkIcon,
	MagnifyingGlassMinusIcon,
	MagnifyingGlassPlusIcon,
	StarIcon,
	XIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {type FC, type ReactNode, type Ref, useCallback, useMemo, useState} from 'react';

interface MobileMediaActionsProps {
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
	enableZoomControls?: boolean;
	onPointerEnter?: () => void;
	onPointerLeave?: () => void;
	rootRef?: Ref<HTMLDivElement>;
}

const iconSize = remFromPx(20);

function compactGroups(groups: Array<MenuGroupType>): Array<MenuGroupType> {
	return groups.filter((group) => group.items.length > 0);
}

export const MobileMediaActions: FC<MobileMediaActionsProps> = observer(
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
		enableZoomControls = false,
		onPointerEnter,
		onPointerLeave,
		rootRef,
	}: MobileMediaActionsProps) => {
		const {i18n} = useLingui();
		const [isSheetOpen, setIsSheetOpen] = useState(false);
		const handleOpenSheet = useCallback(() => {
			setIsSheetOpen(true);
		}, []);
		const handleCloseSheet = useCallback(() => {
			setIsSheetOpen(false);
		}, []);
		const createAction = useCallback(
			(action: () => void) => () => {
				setIsSheetOpen(false);
				action();
			},
			[],
		);
		const createItem = useCallback(
			({
				id,
				icon,
				label,
				action,
				disabled = false,
			}: {
				id: string;
				icon: ReactNode;
				label: string;
				action: () => void;
				disabled?: boolean;
			}): MenuItemType => ({
				id,
				icon,
				label,
				disabled,
				onClick: disabled ? () => undefined : createAction(action),
			}),
			[createAction],
		);
		const favoriteLabel = isFavorited ? i18n._(REMOVE_FROM_FAVORITES_DESCRIPTOR) : i18n._(ADD_TO_FAVORITES_DESCRIPTOR);
		const groups = useMemo(
			() =>
				compactGroups([
					{
						items: [
							...(onOpenInBrowser
								? [
										createItem({
											id: 'media-open-browser',
											icon: (
												<ArrowSquareOutIcon
													size={iconSize}
													weight="bold"
													data-flx="messaging.media-modal.mobile-media-actions.groups.arrow-square-out-icon"
												/>
											),
											label: i18n._(OPEN_IN_BROWSER_DESCRIPTOR),
											action: onOpenInBrowser,
										}),
									]
								: []),
							...(onCopyLink
								? [
										createItem({
											id: 'media-copy-link',
											icon: (
												<LinkIcon
													size={iconSize}
													weight="bold"
													data-flx="messaging.media-modal.mobile-media-actions.groups.link-icon"
												/>
											),
											label: i18n._(COPY_LINK_DESCRIPTOR),
											action: onCopyLink,
										}),
									]
								: []),
							...(onCopyMedia
								? [
										createItem({
											id: 'media-copy-media',
											icon: (
												<CopySimpleIcon
													size={iconSize}
													weight="bold"
													data-flx="messaging.media-modal.mobile-media-actions.groups.copy-simple-icon"
												/>
											),
											label: i18n._(COPY_MEDIA_TO_CLIPBOARD_DESCRIPTOR),
											action: onCopyMedia,
										}),
									]
								: []),
							...(onFavorite
								? [
										createItem({
											id: 'media-favorite',
											icon: (
												<StarIcon
													size={iconSize}
													weight={isFavorited ? 'fill' : 'bold'}
													data-flx="messaging.media-modal.mobile-media-actions.groups.star-icon"
												/>
											),
											label: favoriteLabel,
											action: onFavorite,
										}),
									]
								: []),
							...(onDownload
								? [
										createItem({
											id: 'media-download',
											icon: (
												<DownloadSimpleIcon
													size={iconSize}
													weight="bold"
													data-flx="messaging.media-modal.mobile-media-actions.groups.download-simple-icon"
												/>
											),
											label: i18n._(DOWNLOAD_MEDIA_DESCRIPTOR),
											action: onDownload,
										}),
									]
								: []),
						],
					},
					{
						items: enableZoomControls
							? [
									createItem({
										id: 'media-rotate-ccw',
										icon: (
											<ArrowCounterClockwiseIcon
												size={iconSize}
												weight="bold"
												data-flx="messaging.media-modal.mobile-media-actions.groups.arrow-counter-clockwise-icon"
											/>
										),
										label: i18n._(ROTATE_ANTICLOCKWISE_DESCRIPTOR),
										action: onRotateCCW ?? (() => undefined),
										disabled: !onRotateCCW,
									}),
									createItem({
										id: 'media-rotate-cw',
										icon: (
											<ArrowClockwiseIcon
												size={iconSize}
												weight="bold"
												data-flx="messaging.media-modal.mobile-media-actions.groups.arrow-clockwise-icon"
											/>
										),
										label: i18n._(ROTATE_CLOCKWISE_DESCRIPTOR),
										action: onRotateCW ?? (() => undefined),
										disabled: !onRotateCW,
									}),
									createItem({
										id: 'media-reset-position',
										icon: (
											<ArrowsInSimpleIcon
												size={iconSize}
												weight="bold"
												data-flx="messaging.media-modal.mobile-media-actions.groups.arrows-in-simple-icon"
											/>
										),
										label: i18n._(RESET_MEDIA_POSITION_DESCRIPTOR),
										action: onReset ?? (() => undefined),
										disabled: !canReset || !onReset,
									}),
									createItem({
										id: 'media-zoom-in',
										icon: (
											<MagnifyingGlassPlusIcon
												size={iconSize}
												weight="bold"
												data-flx="messaging.media-modal.mobile-media-actions.groups.magnifying-glass-plus-icon"
											/>
										),
										label: i18n._(ZOOM_IN_DESCRIPTOR),
										action: onZoomIn ?? (() => undefined),
										disabled: !onZoomIn,
									}),
									createItem({
										id: 'media-zoom-out',
										icon: (
											<MagnifyingGlassMinusIcon
												size={iconSize}
												weight="bold"
												data-flx="messaging.media-modal.mobile-media-actions.groups.magnifying-glass-minus-icon"
											/>
										),
										label: i18n._(ZOOM_OUT_DESCRIPTOR),
										action: onZoomOut ?? (() => undefined),
										disabled: !onZoomOut,
									}),
								]
							: [],
					},
					{
						items: [
							...(onReply
								? [
										createItem({
											id: 'media-reply',
											icon: (
												<ArrowBendUpLeftIcon
													size={iconSize}
													weight="bold"
													data-flx="messaging.media-modal.mobile-media-actions.groups.arrow-bend-up-left-icon"
												/>
											),
											label: i18n._(REPLY_TO_MEDIA_MESSAGE_DESCRIPTOR),
											action: onReply,
										}),
									]
								: []),
							...(onForward
								? [
										createItem({
											id: 'media-forward',
											icon: (
												<ArrowBendUpRightIcon
													size={iconSize}
													weight="bold"
													data-flx="messaging.media-modal.mobile-media-actions.groups.arrow-bend-up-right-icon"
												/>
											),
											label: i18n._(FORWARD_MEDIA_DESCRIPTOR),
											action: onForward,
										}),
									]
								: []),
						],
					},
				]),
			[
				canReset,
				createItem,
				enableZoomControls,
				favoriteLabel,
				i18n.locale,
				isFavorited,
				onCopyLink,
				onCopyMedia,
				onFavorite,
				onForward,
				onOpenInBrowser,
				onReply,
				onReset,
				onRotateCCW,
				onRotateCW,
				onDownload,
				onZoomIn,
				onZoomOut,
			],
		);
		return (
			<>
				<div
					ref={rootRef}
					className={styles.mobileOverlayActionBar}
					role="toolbar"
					aria-label={i18n._(MEDIA_CONTROLS_DESCRIPTOR)}
					onPointerEnter={onPointerEnter}
					onPointerLeave={onPointerLeave}
					data-flx="messaging.media-modal.mobile-media-actions.action-bar"
				>
					<ControlButton
						icon={
							<XIcon
								size={remFromPx(22)}
								weight="bold"
								data-flx="messaging.media-modal.mobile-media-actions.close-icon"
							/>
						}
						label={i18n._(CLOSE_MODAL_DESCRIPTOR)}
						onClick={onClose}
						variant="danger"
						data-flx="messaging.media-modal.mobile-media-actions.control-button.close"
					/>
					<ControlButton
						icon={
							<DotsThreeIcon
								size={remFromPx(26)}
								weight="bold"
								data-flx="messaging.media-modal.mobile-media-actions.more-options-icon"
							/>
						}
						label={i18n._(MORE_OPTIONS_DESCRIPTOR)}
						onClick={handleOpenSheet}
						data-flx="messaging.media-modal.mobile-media-actions.control-button.open-sheet"
					/>
				</div>
				<MenuBottomSheet
					isOpen={isSheetOpen}
					onClose={handleCloseSheet}
					title={i18n._(MEDIA_CONTROLS_DESCRIPTOR)}
					groups={groups}
					data-flx="messaging.media-modal.mobile-media-actions.menu-bottom-sheet"
				/>
			</>
		);
	},
);
