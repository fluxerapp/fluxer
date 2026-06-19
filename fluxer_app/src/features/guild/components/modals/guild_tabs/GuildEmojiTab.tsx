// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {GlobalLimits} from '@app/features/app/utils/GlobalLimits';
import {EmojiListHeader, EmojiListItem} from '@app/features/emoji/components/emojis/EmojiListItem';
import {EmojiUploadModal} from '@app/features/emoji/components/modals/EmojiUploadModal';
import EmojiStickerLayout from '@app/features/emoji/state/EmojiStickerLayout';
import * as GuildEmojiCommands from '@app/features/expressions/commands/GuildEmojiCommands';
import {
	seedGuildEmojiCache,
	subscribeToGuildEmojiUpdates,
} from '@app/features/expressions/state/GuildExpressionTabCache';
import {getAcceptString, getAssetFormatErrorMessage} from '@app/features/expressions/utils/AssetFormatCopy';
import * as ImageCropUtils from '@app/features/expressions/utils/ImageCropUtils';
import {isSvgFile} from '@app/features/expressions/utils/ImageUploadFileUtils';
import {CloneAllowedToggle} from '@app/features/guild/components/modals/guild_tabs/CloneAllowedToggle';
import styles from '@app/features/guild/components/modals/guild_tabs/GuildEmojiTab.module.css';
import {UploadDropZone} from '@app/features/guild/components/UploadDropZone';
import {UploadSlotInfo} from '@app/features/guild/components/UploadSlotInfo';
import Guilds from '@app/features/guild/state/Guilds';
import {OKAY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {openFilePicker} from '@app/features/messaging/utils/FilePickerUtils';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import Permission from '@app/features/permissions/state/Permission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {failureValidationErrors} from '@app/features/platform/utils/ResponseInspection';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Spinner} from '@app/features/ui/components/Spinner';
import Users from '@app/features/user/state/Users';
import {canCropFormat} from '@app/features/voice/utils/MediaCapabilities';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MAX_GUILD_EMOJIS} from '@fluxer/constants/src/LimitConstants';
import type {GuildEmojiWithUser} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import {sortBySnowflakeDesc} from '@fluxer/snowflake/src/SnowflakeUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon, WarningCircleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {matchSorter} from 'match-sorter';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';

const NO_EMOJI_SLOTS_AVAILABLE_DESCRIPTOR = msg({
	message: 'No emoji slots available',
	comment: 'Empty-state text in the guild emoji tab.',
});
const EMOJI_SLOTS_FULL_DESCRIPTION_DESCRIPTOR = msg({
	message: "You've reached the maximum number of emojis. Delete some existing emojis to make room.",
	comment: 'Emoji upload limit message in community emoji settings. Do not suggest that communities can be upgraded.',
});
const FAILED_TO_PREPARE_EMOJIS_DESCRIPTOR = msg({
	message: 'Failed to prepare emojis',
	comment: 'Error message in the guild emoji tab.',
});
const FAILED_TO_UPLOAD_EMOJIS_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: 'Failed to upload emojis. Try again.',
	comment: 'Error message in the guild emoji tab.',
});
const FAILED_TO_UPLOAD_EMOJIS_STARTING_AT_DESCRIPTOR = msg({
	message: 'Failed to upload emojis starting at {startLabel}: {errorMsg}',
	comment:
		'Error message in the emoji upload failure modal. startLabel is a chunk label like "#1"; errorMsg is the server error.',
});
const SOME_EMOJIS_COULD_NOT_BE_ADDED_DESCRIPTOR = msg({
	message: "Some emojis couldn't be added",
	comment: 'Title of the error modal shown after some emoji uploads fail.',
});
const SOME_EMOJIS_COULD_NOT_BE_ADDED_BODY_DESCRIPTOR = msg({
	message: 'Review these files and try again with smaller or simpler images.',
	comment: 'Intro text in the error modal shown after some emoji uploads fail.',
});
const EMOJI_SOURCE_FILE_TOO_LARGE_DESCRIPTOR = msg({
	message:
		'This file is {fileSize}, over the {maxSize} limit. Animated emojis and SVGs must already be small enough to upload. Choose a smaller file or shorten the animation.',
	comment:
		'Error text for animated emoji or SVG uploads that exceed the file size limit. {fileSize} and {maxSize} are formatted file sizes.',
});
const EMOJI_PROCESSED_FILE_TOO_LARGE_DESCRIPTOR = msg({
	message:
		'We resized and compressed this emoji to 128x128 pixels, but it is still {fileSize}. The limit is {maxSize}. Try a simpler image, fewer colors, or a smaller source file.',
	comment:
		'Error text for static emoji uploads that still exceed the file size limit after resizing. {fileSize} and {maxSize} are formatted file sizes.',
});
const EMOJI_PREPARATION_FAILED_DESCRIPTOR = msg({
	message: 'This emoji could not be prepared. Try another file or try again in a moment.',
	comment: 'Generic per-file error text shown when preparing an emoji upload fails.',
});
const SEARCH_EMOJIS_DESCRIPTOR = msg({
	message: 'Search emojis',
	comment: 'Button or menu action label in the guild emoji tab. Keep it concise.',
});
const EMOJI_UPLOAD_REQUIREMENTS_DESCRIPTOR = msg({
	message:
		'Emoji names need at least 2 characters and can use letters, numbers, and underscores. Emojis must be under {maxSize}. Static images are resized to 128x128 pixels and compressed automatically. Animated emojis and SVGs must already fit the limit.',
	comment: 'Description in the community emoji upload section. {maxSize} is the formatted size limit.',
});
const NON_ANIMATED_EMOJI_SECTION_TITLE_DESCRIPTOR = msg({
	message: 'Non-animated emoji ({emojiCount})',
	comment: 'Section heading for static emoji in community emoji settings.',
});
const ANIMATED_EMOJI_SECTION_TITLE_DESCRIPTOR = msg({
	message: 'Animated emoji ({emojiCount})',
	comment: 'Section heading for animated emoji in community emoji settings.',
});
const MAX_EMOJIS_PER_UPLOAD = 50;
const logger = new Logger('GuildEmojiTab');

interface EmojiUploadFailure {
	name: string;
	error: string;
}

const GuildEmojiTab: React.FC<{guildId: string}> = observer(function GuildEmojiTab({guildId}) {
	const {i18n} = useLingui();
	const [emojis, setEmojis] = useState<ReadonlyArray<GuildEmojiWithUser>>([]);
	const [fetchStatus, setFetchStatus] = useState<'idle' | 'pending' | 'success' | 'error'>('idle');
	const [searchQuery, setSearchQuery] = useState('');
	const layoutState = EmojiStickerLayout;
	const layout = layoutState.getEmojiLayout();
	const guild = Guilds.getGuild(guildId);
	const canCreateExpressions = Permission.can(Permissions.CREATE_EXPRESSIONS, {guildId});
	const canManageExpressions = Permission.can(Permissions.MANAGE_EXPRESSIONS, {guildId});
	const currentUserId = Users.currentUserId;
	const setEmojisWithCache = useCallback(
		(updater: React.SetStateAction<ReadonlyArray<GuildEmojiWithUser>>) => {
			setEmojis((prev) => {
				const next =
					typeof updater === 'function'
						? (updater as (previous: ReadonlyArray<GuildEmojiWithUser>) => ReadonlyArray<GuildEmojiWithUser>)(prev)
						: updater;
				const frozen = Object.freeze(sortBySnowflakeDesc(next));
				seedGuildEmojiCache(guildId, frozen);
				return frozen;
			});
		},
		[guildId],
	);
	const fetchEmojis = useCallback(async () => {
		try {
			setFetchStatus('pending');
			const emojiList = await GuildEmojiCommands.list(guildId);
			setEmojisWithCache(emojiList);
			setFetchStatus('success');
		} catch (error) {
			logger.error('Failed to fetch emojis', error);
			setFetchStatus('error');
		}
	}, [guildId, setEmojisWithCache]);
	useEffect(() => {
		if (fetchStatus === 'idle') {
			void fetchEmojis();
		}
	}, [fetchStatus, fetchEmojis]);
	useEffect(() => {
		return subscribeToGuildEmojiUpdates(guildId, (updatedEmojis) => {
			setEmojisWithCache(updatedEmojis);
		});
	}, [guildId, setEmojisWithCache]);
	const filteredEmojis = useMemo(() => {
		if (!searchQuery) return emojis;
		return matchSorter(emojis, searchQuery, {keys: ['name']});
	}, [emojis, searchQuery]);
	const {staticEmojis, animatedEmojis} = useMemo(() => {
		const statics: Array<GuildEmojiWithUser> = [];
		const animateds: Array<GuildEmojiWithUser> = [];
		for (const emoji of filteredEmojis) {
			if (emoji.animated) {
				animateds.push(emoji);
			} else {
				statics.push(emoji);
			}
		}
		return {staticEmojis: statics, animatedEmojis: animateds};
	}, [filteredEmojis]);
	const maxEmojis = guild?.maxEmojis ?? MAX_GUILD_EMOJIS;
	const currentEmojiCount = emojis.length;
	const emojiMaxSizeLabel = formatFileSize(GlobalLimits.getEmojiMaxSize());
	const canModifyEmoji = useCallback(
		(emoji: GuildEmojiWithUser): boolean => {
			if (canManageExpressions) return true;
			if (canCreateExpressions && emoji.user?.id === currentUserId) return true;
			return false;
		},
		[canManageExpressions, canCreateExpressions, currentUserId],
	);
	const handleEmojiDelete = useCallback((emojiId: string) => {
		setEmojis((prev) => prev.filter((e) => e.id !== emojiId));
	}, []);
	const handleEmojiRename = useCallback(
		async (emojiId: string, newName: string) => {
			try {
				await GuildEmojiCommands.update(guildId, emojiId, {name: newName});
				setEmojis((prev) => prev.map((e) => (e.id === emojiId ? {...e, name: newName} : e)));
			} catch (error) {
				logger.error('Failed to rename emoji', error);
			}
		},
		[guildId],
	);
	const renderEmojiFailureDescription = useCallback(
		(intro: React.ReactNode, failures: ReadonlyArray<EmojiUploadFailure>) => (
			<div className={styles.modalErrorContainer} data-flx="guild.guild-tabs.guild-emoji-tab.emoji-failures">
				<p className={styles.modalErrorIntro} data-flx="guild.guild-tabs.guild-emoji-tab.emoji-failures.intro">
					{intro}
				</p>
				{failures.map((failed, index) => (
					<div
						key={`${failed.name}-${index}`}
						className={styles.modalErrorItem}
						data-flx="guild.guild-tabs.guild-emoji-tab.emoji-failures.item"
					>
						<div
							className={styles.modalErrorDetails}
							data-flx="guild.guild-tabs.guild-emoji-tab.emoji-failures.details"
						>
							<div className={styles.modalErrorName} data-flx="guild.guild-tabs.guild-emoji-tab.emoji-failures.name">
								{failed.name}
							</div>
							<div
								className={styles.modalErrorMessage}
								data-flx="guild.guild-tabs.guild-emoji-tab.emoji-failures.message"
							>
								{failed.error}
							</div>
						</div>
					</div>
				))}
			</div>
		),
		[],
	);
	const handleFileSelect = useCallback(
		async (files: FileList | ReadonlyArray<File>) => {
			const candidateFiles = Array.from(files);
			const decodableFiles: Array<File> = [];
			const validationFailures: Array<EmojiUploadFailure> = [];
			for (const file of candidateFiles) {
				if (isSvgFile(file) || (await canCropFormat(file.type))) {
					decodableFiles.push(file);
				} else {
					validationFailures.push({
						name: file.name,
						error: getAssetFormatErrorMessage(i18n, 'emoji', 'unsupported_mime'),
					});
				}
			}
			const availableSlots = maxEmojis - currentEmojiCount;
			if (availableSlots <= 0) {
				ModalCommands.push(
					modal(() => (
						<GenericErrorModal
							title={i18n._(NO_EMOJI_SLOTS_AVAILABLE_DESCRIPTOR)}
							message={i18n._(EMOJI_SLOTS_FULL_DESCRIPTION_DESCRIPTOR)}
							data-flx="guild.guild-tabs.guild-emoji-tab.handle-file-select.confirm-modal"
						/>
					)),
				);
				return;
			}
			const filesWithinSlots = decodableFiles.slice(0, availableSlots);
			if (filesWithinSlots.length === 0) {
				ModalCommands.push(
					modal(() => (
						<ConfirmModal
							title={i18n._(FAILED_TO_PREPARE_EMOJIS_DESCRIPTOR)}
							description={renderEmojiFailureDescription(
								<Trans>Unable to prepare any emojis for upload. The following errors occurred:</Trans>,
								validationFailures,
							)}
							primaryText={i18n._(OKAY_DESCRIPTOR)}
							primaryVariant="primary"
							onPrimary={() => {}}
							data-flx="guild.guild-tabs.guild-emoji-tab.handle-file-select.unsupported-files-confirm-modal"
						/>
					)),
				);
				return;
			}
			ModalCommands.push(
				modal(() => (
					<EmojiUploadModal
						count={filesWithinSlots.length}
						data-flx="guild.guild-tabs.guild-emoji-tab.handle-file-select.emoji-upload-modal"
					/>
				)),
			);
			const preparedEmojis: Array<{name: string; image: string; file: File}> = [];
			const preparationFailures: Array<EmojiUploadFailure> = [];
			const maxEmojiSize = GlobalLimits.getEmojiMaxSize();
			const maxEmojiSizeLabel = formatFileSize(maxEmojiSize);
			for (const file of filesWithinSlots) {
				try {
					const base64Image = await ImageCropUtils.optimizeEmojiImage(file, maxEmojiSize, 128);
					const name = GuildEmojiCommands.sanitizeEmojiName(file.name);
					preparedEmojis.push({name, image: base64Image, file});
				} catch (error) {
					logger.error(`Failed to prepare emoji ${file.name}`, error);
					const errorMessage =
						error instanceof ImageCropUtils.ImageOptimizationSizeError
							? i18n._(
									error.reason === 'processed'
										? EMOJI_PROCESSED_FILE_TOO_LARGE_DESCRIPTOR
										: EMOJI_SOURCE_FILE_TOO_LARGE_DESCRIPTOR,
									{
										fileSize: formatFileSize(error.actualSizeBytes),
										maxSize: maxEmojiSizeLabel,
									},
								)
							: i18n._(EMOJI_PREPARATION_FAILED_DESCRIPTOR);
					preparationFailures.push({
						name: file.name,
						error: errorMessage,
					});
				}
			}
			if (preparedEmojis.length === 0) {
				ModalCommands.pop();
				ModalCommands.push(
					modal(() => (
						<ConfirmModal
							title={i18n._(FAILED_TO_PREPARE_EMOJIS_DESCRIPTOR)}
							description={renderEmojiFailureDescription(
								<Trans>Unable to prepare any emojis for upload. The following errors occurred:</Trans>,
								preparationFailures,
							)}
							primaryText={i18n._(OKAY_DESCRIPTOR)}
							primaryVariant="primary"
							onPrimary={() => {}}
							data-flx="guild.guild-tabs.guild-emoji-tab.handle-file-select.confirm-modal--2"
						/>
					)),
				);
				return;
			}
			const chunkedPreparedEmojis: Array<{
				chunk: Array<{name: string; image: string; file: File}>;
				startIndex: number;
			}> = [];
			for (let chunkStart = 0; chunkStart < preparedEmojis.length; chunkStart += MAX_EMOJIS_PER_UPLOAD) {
				chunkedPreparedEmojis.push({
					chunk: preparedEmojis.slice(chunkStart, chunkStart + MAX_EMOJIS_PER_UPLOAD),
					startIndex: chunkStart,
				});
			}
			let hasUploadError = false;
			let hasUploadedEmoji = false;
			const uploadFailures: Array<EmojiUploadFailure> = [];
			for (const {chunk, startIndex} of chunkedPreparedEmojis) {
				try {
					const emojisToUpload = chunk.map(({name, image}) => ({name, image}));
					const result = await GuildEmojiCommands.bulkUpload(guildId, emojisToUpload);
					if (result.success.length > 0) {
						hasUploadedEmoji = true;
					}
					if (result.failed.length > 0) {
						hasUploadError = true;
						uploadFailures.push(...result.failed);
					}
				} catch (error: unknown) {
					hasUploadError = true;
					logger.error(`Failed to upload emojis starting at index ${startIndex}`, error);
					const validationErrors = failureValidationErrors(error);
					const errorMsg =
						validationErrors?.map((e) => e.message).join(', ') ??
						(error instanceof Error ? error.message : null) ??
						i18n._(FAILED_TO_UPLOAD_EMOJIS_PLEASE_TRY_AGAIN_DESCRIPTOR);
					const startLabel = `#${startIndex + 1}`;
					uploadFailures.push({
						name: startLabel,
						error: i18n._(FAILED_TO_UPLOAD_EMOJIS_STARTING_AT_DESCRIPTOR, {startLabel, errorMsg}),
					});
				}
			}
			if (!hasUploadError || hasUploadedEmoji) {
				await fetchEmojis();
			}
			ModalCommands.pop();
			const failures = [...validationFailures, ...preparationFailures, ...uploadFailures];
			if (failures.length > 0) {
				ModalCommands.push(
					modal(() => (
						<ConfirmModal
							title={i18n._(SOME_EMOJIS_COULD_NOT_BE_ADDED_DESCRIPTOR)}
							description={renderEmojiFailureDescription(
								i18n._(SOME_EMOJIS_COULD_NOT_BE_ADDED_BODY_DESCRIPTOR),
								failures,
							)}
							primaryText={i18n._(OKAY_DESCRIPTOR)}
							primaryVariant="primary"
							onPrimary={() => {}}
							data-flx="guild.guild-tabs.guild-emoji-tab.handle-file-select.partial-failure-modal"
						/>
					)),
				);
			}
		},
		[guildId, fetchEmojis, maxEmojis, currentEmojiCount, i18n, renderEmojiFailureDescription],
	);
	const handleDrop = useCallback(
		async (files: Array<File>) => {
			if (files.length > 0) {
				void handleFileSelect(files);
			}
		},
		[handleFileSelect],
	);
	return (
		<div className={styles.container} data-flx="guild.guild-tabs.guild-emoji-tab.container">
			<CloneAllowedToggle
				guildId={guildId}
				kind="emoji"
				data-flx="guild.guild-tabs.guild-emoji-tab.clone-allowed-toggle"
			/>
			<div className={styles.controls} data-flx="guild.guild-tabs.guild-emoji-tab.controls">
				<Input
					type="text"
					placeholder={i18n._(SEARCH_EMOJIS_DESCRIPTOR)}
					value={searchQuery}
					onChange={(e) => setSearchQuery(e.target.value)}
					leftIcon={
						<MagnifyingGlassIcon
							size={16}
							weight="bold"
							data-flx="guild.guild-tabs.guild-emoji-tab.magnifying-glass-icon"
						/>
					}
					className={styles.searchInput}
					data-flx="guild.guild-tabs.guild-emoji-tab.search-input.set-search-query.text"
				/>
			</div>
			{fetchStatus === 'error' && (
				<div className={styles.notice} data-flx="guild.guild-tabs.guild-emoji-tab.notice">
					<p className={styles.noticeText} data-flx="guild.guild-tabs.guild-emoji-tab.notice-text">
						<WarningCircleIcon
							size={32}
							weight="fill"
							data-flx="guild.guild-tabs.guild-emoji-tab.warning-circle-icon"
						/>
						<Trans>Failed to load emojis. Try again later.</Trans>
					</p>
				</div>
			)}
			{canCreateExpressions && (
				<>
					<UploadSlotInfo
						title={<Trans>Emoji slots</Trans>}
						currentCount={currentEmojiCount}
						maxCount={maxEmojis}
						uploadButtonText={<Trans>Upload emoji</Trans>}
						onUploadClick={async () => {
							const files = await openFilePicker({
								multiple: true,
								accept: getAcceptString('emoji'),
							});
							if (files.length > 0) {
								void handleFileSelect(files);
							}
						}}
						description={i18n._(EMOJI_UPLOAD_REQUIREMENTS_DESCRIPTOR, {
							maxSize: emojiMaxSizeLabel,
						})}
						data-flx="guild.guild-tabs.guild-emoji-tab.upload-slot-info"
					/>
					<UploadDropZone
						onDrop={handleDrop}
						description={<Trans>Drag and drop emoji files here</Trans>}
						data-flx="guild.guild-tabs.guild-emoji-tab.upload-drop-zone"
					/>
				</>
			)}
			{searchQuery && filteredEmojis.length === 0 && (
				<div className={styles.notice} data-flx="guild.guild-tabs.guild-emoji-tab.notice--2">
					<p className={styles.noticeText} data-flx="guild.guild-tabs.guild-emoji-tab.notice-text--2">
						<Trans>No emojis found matching your search.</Trans>
					</p>
				</div>
			)}
			{fetchStatus === 'pending' && (
				<div className={styles.spinnerContainer} data-flx="guild.guild-tabs.guild-emoji-tab.spinner-container">
					<Spinner data-flx="guild.guild-tabs.guild-emoji-tab.spinner" />
				</div>
			)}
			{fetchStatus === 'success' && (staticEmojis.length > 0 || animatedEmojis.length > 0) && (
				<div
					className={clsx(styles.emojiSections, layout === 'grid' && styles.emojiSectionsGrid)}
					data-flx="guild.guild-tabs.guild-emoji-tab.emoji-sections"
				>
					{staticEmojis.length > 0 && (
						<div className={styles.emojiSection} data-flx="guild.guild-tabs.guild-emoji-tab.emoji-section">
							<h3 className={styles.emojiSectionTitle} data-flx="guild.guild-tabs.guild-emoji-tab.emoji-section-title">
								{i18n._(NON_ANIMATED_EMOJI_SECTION_TITLE_DESCRIPTOR, {emojiCount: staticEmojis.length})}
							</h3>
							{layout === 'list' && <EmojiListHeader data-flx="guild.guild-tabs.guild-emoji-tab.emoji-list-header" />}
							<div
								className={layout === 'list' ? styles.emojiItemsList : styles.emojiGrid}
								data-flx="guild.guild-tabs.guild-emoji-tab.emoji-items-list"
							>
								{staticEmojis.map((emoji: GuildEmojiWithUser) => (
									<EmojiListItem
										key={emoji.id}
										guildId={guildId}
										emoji={emoji}
										layout={layout}
										canModify={canModifyEmoji(emoji)}
										onRename={handleEmojiRename}
										onRemove={handleEmojiDelete}
										data-flx="guild.guild-tabs.guild-emoji-tab.emoji-list-item"
									/>
								))}
							</div>
						</div>
					)}
					{animatedEmojis.length > 0 && (
						<div className={styles.emojiSection} data-flx="guild.guild-tabs.guild-emoji-tab.emoji-section--2">
							<h3
								className={styles.emojiSectionTitle}
								data-flx="guild.guild-tabs.guild-emoji-tab.emoji-section-title--2"
							>
								{i18n._(ANIMATED_EMOJI_SECTION_TITLE_DESCRIPTOR, {emojiCount: animatedEmojis.length})}
							</h3>
							{layout === 'list' && (
								<EmojiListHeader data-flx="guild.guild-tabs.guild-emoji-tab.emoji-list-header--2" />
							)}
							<div
								className={layout === 'list' ? styles.emojiItemsList : styles.emojiGrid}
								data-flx="guild.guild-tabs.guild-emoji-tab.emoji-items-list--2"
							>
								{animatedEmojis.map((emoji: GuildEmojiWithUser) => (
									<EmojiListItem
										key={emoji.id}
										guildId={guildId}
										emoji={emoji}
										layout={layout}
										canModify={canModifyEmoji(emoji)}
										onRename={handleEmojiRename}
										onRemove={handleEmojiDelete}
										data-flx="guild.guild-tabs.guild-emoji-tab.emoji-list-item--2"
									/>
								))}
							</div>
						</div>
					)}
				</div>
			)}
		</div>
	);
});

export default GuildEmojiTab;
