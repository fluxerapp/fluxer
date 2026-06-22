// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {StatusSlate} from '@app/features/app/components/dialogs/shared/StatusSlate';
import {GlobalLimits} from '@app/features/app/utils/GlobalLimits';
import {StickerGridItem} from '@app/features/emoji/components/stickers/StickerGridItem';
import EmojiStickerLayout from '@app/features/emoji/state/EmojiStickerLayout';
import * as GuildStickerCommands from '@app/features/expressions/commands/GuildStickerCommands';
import {AddGuildStickerModal} from '@app/features/expressions/components/modals/AddGuildStickerModal';
import {
	seedGuildStickerCache,
	subscribeToGuildStickerUpdates,
} from '@app/features/expressions/state/GuildExpressionTabCache';
import {getAcceptString, getAssetFormatErrorMessage} from '@app/features/expressions/utils/AssetFormatCopy';
import {isSvgFile} from '@app/features/expressions/utils/ImageUploadFileUtils';
import {CloneAllowedToggle} from '@app/features/guild/components/modals/guild_tabs/CloneAllowedToggle';
import styles from '@app/features/guild/components/modals/guild_tabs/GuildStickersTab.module.css';
import {UploadDropZone} from '@app/features/guild/components/UploadDropZone';
import {UploadSlotInfo} from '@app/features/guild/components/UploadSlotInfo';
import Guilds from '@app/features/guild/state/Guilds';
import {OKAY_DESCRIPTOR, TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {openFilePicker} from '@app/features/messaging/utils/FilePickerUtils';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import Permission from '@app/features/permissions/state/Permission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Spinner} from '@app/features/ui/components/Spinner';
import Users from '@app/features/user/state/Users';
import {canCropFormat} from '@app/features/voice/utils/MediaCapabilities';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MAX_GUILD_STICKERS} from '@fluxer/constants/src/LimitConstants';
import type {GuildStickerWithUser} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import {sortBySnowflakeDesc} from '@fluxer/snowflake/src/SnowflakeUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon, WarningCircleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {matchSorter} from 'match-sorter';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';

const SEARCH_STICKERS_DESCRIPTOR = msg({
	message: 'Search stickers',
	comment: 'Button or menu action label in the guild stickers tab. Keep it concise.',
});
const STICKER_DENSITY_DESCRIPTOR = msg({
	message: 'Sticker density',
	comment: 'Short label in the guild stickers tab. Keep it concise.',
});
const NO_STICKERS_FOUND_DESCRIPTOR = msg({
	message: 'No stickers found',
	comment: 'Empty-state text in the guild stickers tab.',
});
const NO_STICKER_SLOTS_AVAILABLE_DESCRIPTOR = msg({
	message: 'No sticker slots available',
	comment: 'Empty-state text in the guild stickers tab.',
});
const STICKER_SLOTS_FULL_DESCRIPTION_DESCRIPTOR = msg({
	message: "You've reached the maximum number of stickers. Delete some existing stickers to make room.",
	comment:
		'Sticker upload limit message in community sticker settings. Do not suggest that communities can be upgraded.',
});
const NO_STICKERS_FOUND_MATCHING_YOUR_SEARCH_DESCRIPTOR = msg({
	message: 'No stickers found matching your search.',
	comment: 'Empty-state text in the guild stickers tab.',
});
const FAILED_TO_LOAD_STICKERS_DESCRIPTOR = msg({
	message: 'Failed to load stickers',
	comment: 'Error message in the guild stickers tab.',
});
const THERE_WAS_AN_ERROR_LOADING_THE_STICKERS_PLEASE_DESCRIPTOR = msg({
	message: 'There was an error loading the stickers. Try again.',
	comment: 'Error message in the guild stickers tab.',
});
const STICKER_UPLOAD_REQUIREMENTS_DESCRIPTOR = msg({
	message:
		'Stickers are saved at 320x320 pixels and must be under {maxSize}. Static images are resized and compressed automatically. Animated stickers and SVGs must already fit the limit.',
	comment: 'Description in the community sticker upload section. {maxSize} is the formatted size limit.',
});
const UNSUPPORTED_STICKER_FILE_DESCRIPTOR = msg({
	message: 'Unsupported sticker file',
	comment: 'Title of the error modal shown when an unsupported sticker file type is selected.',
});
const logger = new Logger('GuildStickersTab');
const GuildStickersTab: React.FC<{guildId: string}> = observer(function GuildStickersTab({guildId}) {
	const {i18n} = useLingui();
	const [stickers, setStickers] = useState<ReadonlyArray<GuildStickerWithUser>>([]);
	const [fetchStatus, setFetchStatus] = useState<'idle' | 'pending' | 'success' | 'error'>('idle');
	const [searchQuery, setSearchQuery] = useState('');
	const layoutState = EmojiStickerLayout;
	const viewMode = layoutState.getStickerViewMode();
	const guild = Guilds.getGuild(guildId);
	const canCreateExpressions = Permission.can(Permissions.CREATE_EXPRESSIONS, {guildId});
	const canManageExpressions = Permission.can(Permissions.MANAGE_EXPRESSIONS, {guildId});
	const currentUserId = Users.currentUserId;
	const setStickersWithCache = useCallback(
		(updater: React.SetStateAction<ReadonlyArray<GuildStickerWithUser>>) => {
			setStickers((prev) => {
				const next =
					typeof updater === 'function'
						? (updater as (previous: ReadonlyArray<GuildStickerWithUser>) => ReadonlyArray<GuildStickerWithUser>)(prev)
						: updater;
				const frozen = Object.freeze(sortBySnowflakeDesc(next));
				seedGuildStickerCache(guildId, frozen);
				return frozen;
			});
		},
		[guildId],
	);
	const fetchStickers = useCallback(async () => {
		try {
			setFetchStatus('pending');
			const stickerList = await GuildStickerCommands.list(guildId);
			setStickersWithCache(stickerList);
			setFetchStatus('success');
		} catch (error) {
			logger.error('Failed to fetch stickers', error);
			setFetchStatus('error');
		}
	}, [guildId, setStickersWithCache]);
	useEffect(() => {
		if (fetchStatus === 'idle') {
			void fetchStickers();
		}
	}, [fetchStatus, fetchStickers]);
	useEffect(() => {
		return subscribeToGuildStickerUpdates(guildId, (updatedStickers) => {
			setStickersWithCache(updatedStickers);
		});
	}, [guildId, setStickersWithCache]);
	const showUnsupportedStickerModal = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(UNSUPPORTED_STICKER_FILE_DESCRIPTOR)}
					description={getAssetFormatErrorMessage(i18n, 'sticker', 'unsupported_mime')}
					primaryText={i18n._(OKAY_DESCRIPTOR)}
					primaryVariant="primary"
					onPrimary={() => {}}
					data-flx="guild.guild-tabs.guild-stickers-tab.unsupported-file-confirm-modal"
				/>
			)),
		);
	}, [i18n]);
	const acceptOrReject = useCallback(
		async (file: File): Promise<boolean> => {
			if (isSvgFile(file)) return true;
			if (await canCropFormat(file.type)) return true;
			showUnsupportedStickerModal();
			return false;
		},
		[showUnsupportedStickerModal],
	);
	const maxStickers = guild?.maxStickers ?? MAX_GUILD_STICKERS;
	const currentStickerCount = stickers.length;
	const showNoStickerSlotsModal = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<GenericErrorModal
					title={i18n._(NO_STICKER_SLOTS_AVAILABLE_DESCRIPTOR)}
					message={i18n._(STICKER_SLOTS_FULL_DESCRIPTION_DESCRIPTOR)}
					data-flx="guild.guild-tabs.guild-stickers-tab.no-sticker-slots-confirm-modal"
				/>
			)),
		);
	}, [i18n]);
	const handleAddSticker = async () => {
		if (currentStickerCount >= maxStickers) {
			showNoStickerSlotsModal();
			return;
		}
		const [file] = await openFilePicker({
			accept: getAcceptString('sticker'),
		});
		if (file && (await acceptOrReject(file))) {
			ModalCommands.push(
				modal(() => (
					<AddGuildStickerModal
						guildId={guildId}
						file={file}
						onSuccess={fetchStickers}
						data-flx="guild.guild-tabs.guild-stickers-tab.handle-add-sticker.add-guild-sticker-modal"
					/>
				)),
			);
		}
	};
	const handleDrop = async (files: Array<File>) => {
		if (currentStickerCount >= maxStickers) {
			showNoStickerSlotsModal();
			return;
		}
		const file = files[0];
		if (file && (await acceptOrReject(file))) {
			ModalCommands.push(
				modal(() => (
					<AddGuildStickerModal
						guildId={guildId}
						file={file}
						onSuccess={fetchStickers}
						data-flx="guild.guild-tabs.guild-stickers-tab.handle-drop.add-guild-sticker-modal"
					/>
				)),
			);
		}
	};
	const filteredStickers = useMemo(() => {
		if (!searchQuery) return stickers;
		return matchSorter(stickers, searchQuery, {
			keys: [(sticker) => sticker.name],
		});
	}, [stickers, searchQuery]);
	const canModifySticker = useCallback(
		(sticker: GuildStickerWithUser): boolean => {
			if (canManageExpressions) return true;
			if (canCreateExpressions && sticker.user?.id === currentUserId) return true;
			return false;
		},
		[canManageExpressions, canCreateExpressions, currentUserId],
	);
	const stickerMaxSizeLabel = formatFileSize(GlobalLimits.getStickerMaxSize());
	return (
		<div className={styles.container} data-flx="guild.guild-tabs.guild-stickers-tab.container">
			<CloneAllowedToggle
				guildId={guildId}
				kind="sticker"
				data-flx="guild.guild-tabs.guild-stickers-tab.clone-allowed-toggle"
			/>
			<div className={styles.controls} data-flx="guild.guild-tabs.guild-stickers-tab.controls">
				<Input
					type="text"
					placeholder={i18n._(SEARCH_STICKERS_DESCRIPTOR)}
					value={searchQuery}
					onChange={(e) => setSearchQuery(e.target.value)}
					leftIcon={
						<MagnifyingGlassIcon
							size={16}
							weight="bold"
							data-flx="guild.guild-tabs.guild-stickers-tab.magnifying-glass-icon"
						/>
					}
					className={styles.searchInput}
					data-flx="guild.guild-tabs.guild-stickers-tab.search-input.set-search-query.text"
				/>
				<div
					className={styles.viewToggle}
					role="group"
					aria-label={i18n._(STICKER_DENSITY_DESCRIPTOR)}
					data-flx="guild.guild-tabs.guild-stickers-tab.view-toggle"
				>
					<button
						type="button"
						onClick={() => layoutState.setStickerViewMode('cozy')}
						className={clsx(styles.viewToggleButton, viewMode === 'cozy' && styles.viewToggleButtonActive)}
						aria-pressed={viewMode === 'cozy'}
						data-flx="guild.guild-tabs.guild-stickers-tab.view-toggle-button.set-sticker-view-mode"
					>
						<Trans>Cozy</Trans>
					</button>
					<button
						type="button"
						onClick={() => layoutState.setStickerViewMode('compact')}
						className={clsx(styles.viewToggleButton, viewMode === 'compact' && styles.viewToggleButtonActive)}
						aria-pressed={viewMode === 'compact'}
						data-flx="guild.guild-tabs.guild-stickers-tab.view-toggle-button.set-sticker-view-mode--2"
					>
						<Trans>Compact</Trans>
					</button>
				</div>
			</div>
			{canCreateExpressions && (
				<>
					<UploadSlotInfo
						title={<Trans>Sticker slots</Trans>}
						currentCount={currentStickerCount}
						maxCount={maxStickers}
						uploadButtonText={<Trans>Upload sticker</Trans>}
						onUploadClick={handleAddSticker}
						description={i18n._(STICKER_UPLOAD_REQUIREMENTS_DESCRIPTOR, {
							maxSize: stickerMaxSizeLabel,
						})}
						data-flx="guild.guild-tabs.guild-stickers-tab.upload-slot-info"
					/>
					<UploadDropZone
						onDrop={handleDrop}
						description={<Trans>Drag and drop a sticker file here (one at a time)</Trans>}
						acceptMultiple={false}
						data-flx="guild.guild-tabs.guild-stickers-tab.upload-drop-zone"
					/>
				</>
			)}
			{fetchStatus === 'pending' && (
				<div className={styles.spinnerContainer} data-flx="guild.guild-tabs.guild-stickers-tab.spinner-container">
					<Spinner data-flx="guild.guild-tabs.guild-stickers-tab.spinner" />
				</div>
			)}
			{searchQuery && filteredStickers.length === 0 && (
				<StatusSlate
					Icon={MagnifyingGlassIcon}
					title={i18n._(NO_STICKERS_FOUND_DESCRIPTOR)}
					description={i18n._(NO_STICKERS_FOUND_MATCHING_YOUR_SEARCH_DESCRIPTOR)}
					fullHeight={true}
					data-flx="guild.guild-tabs.guild-stickers-tab.status-slate"
				/>
			)}
			{fetchStatus === 'success' && filteredStickers.length > 0 && (
				<div
					className={clsx(styles.stickerGrid, viewMode === 'compact' ? styles.compactGrid : styles.cozyGrid)}
					data-flx="guild.guild-tabs.guild-stickers-tab.sticker-grid"
				>
					{filteredStickers.map((sticker) => (
						<StickerGridItem
							key={sticker.id}
							guildId={guildId}
							sticker={sticker}
							canModify={canModifySticker(sticker)}
							onUpdate={fetchStickers}
							data-flx="guild.guild-tabs.guild-stickers-tab.sticker-grid-item"
						/>
					))}
				</div>
			)}
			{fetchStatus === 'error' && (
				<StatusSlate
					Icon={WarningCircleIcon}
					title={i18n._(FAILED_TO_LOAD_STICKERS_DESCRIPTOR)}
					description={i18n._(THERE_WAS_AN_ERROR_LOADING_THE_STICKERS_PLEASE_DESCRIPTOR)}
					actions={[
						{
							text: i18n._(TRY_AGAIN_DESCRIPTOR),
							onClick: fetchStickers,
							variant: 'primary',
						},
					]}
					fullHeight={true}
					data-flx="guild.guild-tabs.guild-stickers-tab.status-slate--2"
				/>
			)}
		</div>
	);
});

export default GuildStickersTab;
