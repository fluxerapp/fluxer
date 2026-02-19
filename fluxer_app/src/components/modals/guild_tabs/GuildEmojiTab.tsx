/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import * as GuildEmojiActionCreators from '@app/actions/GuildEmojiActionCreators';
import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import {modal} from '@app/actions/ModalActionCreators';
import * as ToastActionCreators from '@app/actions/ToastActionCreators';
import {EmojiListHeader, EmojiListItem} from '@app/components/emojis/EmojiListItem';
import {Input} from '@app/components/form/Input';
import {UploadDropZone} from '@app/components/guild/UploadDropZone';
import {UploadSlotInfo} from '@app/components/guild/UploadSlotInfo';
import {ConfirmModal} from '@app/components/modals/ConfirmModal';
import {EmojiUploadModal} from '@app/components/modals/EmojiUploadModal';
import styles from '@app/components/modals/guild_tabs/GuildEmojiTab.module.css';
import {Spinner} from '@app/components/uikit/Spinner';
import {Logger} from '@app/lib/Logger';
import EmojiStickerLayoutStore from '@app/stores/EmojiStickerLayoutStore';
import {seedGuildEmojiCache, subscribeToGuildEmojiUpdates} from '@app/stores/GuildExpressionTabCache';
import GuildStore from '@app/stores/GuildStore';
import PermissionStore from '@app/stores/PermissionStore';
import UserStore from '@app/stores/UserStore';
import {getApiErrorCode, getApiErrorErrors} from '@app/utils/ApiErrorUtils';
import {openFilePicker} from '@app/utils/FilePickerUtils';
import * as ImageCropUtils from '@app/utils/ImageCropUtils';
import {GlobalLimits} from '@app/utils/limits/GlobalLimits';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {GuildEmojiWithUser} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import {sortBySnowflakeDesc} from '@fluxer/snowflake/src/SnowflakeUtils';
import {Trans, useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon, WarningCircleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {matchSorter} from 'match-sorter';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';

const MAX_EMOJIS_PER_UPLOAD = 50;

const logger = new Logger('GuildEmojiTab');

const GuildEmojiTab: React.FC<{guildId: string}> = observer(function GuildEmojiTab({guildId}) {
	const {t} = useLingui();
	const [emojis, setEmojis] = useState<ReadonlyArray<GuildEmojiWithUser>>([]);
	const [fetchStatus, setFetchStatus] = useState<'idle' | 'pending' | 'success' | 'error'>('idle');
	const [searchQuery, setSearchQuery] = useState('');

	const layoutStore = EmojiStickerLayoutStore;
	const layout = layoutStore.getEmojiLayout();
	const guild = GuildStore.getGuild(guildId);

	const canCreateExpressions = PermissionStore.can(Permissions.CREATE_EXPRESSIONS, {guildId});
	const canManageExpressions = PermissionStore.can(Permissions.MANAGE_EXPRESSIONS, {guildId});
	const currentUserId = UserStore.currentUserId;

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
			const emojiList = await GuildEmojiActionCreators.list(guildId);
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

	const maxStaticEmojis = useMemo(() => {
		if (!guild) return 50;
		if (guild.features.has('MORE_EMOJI')) return 250;
		return 50;
	}, [guild]);

	const maxAnimatedEmojis = useMemo(() => {
		if (!guild) return 50;
		if (guild.features.has('MORE_EMOJI')) return 250;
		return 50;
	}, [guild]);

	const canModifyEmoji = useCallback(
		(emoji: GuildEmojiWithUser): boolean => {
			if (canManageExpressions) return true;
			if (canCreateExpressions && emoji.user?.id === currentUserId) return true;
			return false;
		},
		[canManageExpressions, canCreateExpressions, currentUserId],
	);

	const handleEmojiDelete = useCallback(
		async (emojiId: string) => {
			try {
				await GuildEmojiActionCreators.remove(guildId, emojiId);
				setEmojis((prev) => prev.filter((e) => e.id !== emojiId));
			} catch (error: unknown) {
				const errorCode = getApiErrorCode(error);
				if (errorCode === APIErrorCodes.UNKNOWN_EMOJI || errorCode === APIErrorCodes.MISSING_ACCESS) {
					const validationErrors = getApiErrorErrors(error);
					ModalActionCreators.push(
						modal(() => (
							<ConfirmModal
								title={t`Delete Emoji Failed`}
								description={
									validationErrors?.map((e) => e.message).join(', ') || t`Failed to delete emoji. Please try again.`
								}
								primaryText={t`OK`}
								onPrimary={() => {}}
							/>
						)),
					);
				}
			}
		},
		[guildId],
	);

	const handleEmojiRename = useCallback(
		async (emojiId: string, newName: string) => {
			try {
				await GuildEmojiActionCreators.update(guildId, emojiId, {name: newName});
				setEmojis((prev) => prev.map((e) => (e.id === emojiId ? {...e, name: newName} : e)));
			} catch (error) {
				logger.error('Failed to rename emoji', error);
			}
		},
		[guildId],
	);

	const handleFileSelect = useCallback(
		async (files: FileList | ReadonlyArray<File>) => {
			const maxStatic = maxStaticEmojis;
			const statics = filteredEmojis.filter((e) => !e.animated);

			const filesWithinSlots = Array.from(files).slice(0, maxStatic - statics.length);

			if (filesWithinSlots.length === 0) {
				ModalActionCreators.push(
					modal(() => (
						<ConfirmModal
							title={t`No emoji slots available`}
							description={
								<Trans>
									You've reached the maximum number of static emojis. Delete some existing emojis or upgrade your server
									to increase the limit.
								</Trans>
							}
							primaryText={t`Understood`}
							onPrimary={() => {}}
						/>
					)),
				);
				return;
			}

			ModalActionCreators.push(modal(() => <EmojiUploadModal count={filesWithinSlots.length} />));

			const preparedEmojis: Array<{name: string; image: string; file: File}> = [];
			const preparationFailures: Array<{name: string; error: string}> = [];

			for (const file of filesWithinSlots) {
				try {
					const maxEmojiSize = GlobalLimits.getEmojiMaxSize();
					const base64Image = await ImageCropUtils.optimizeEmojiImage(file, maxEmojiSize, 128);
					const name = GuildEmojiActionCreators.sanitizeEmojiName(file.name);
					preparedEmojis.push({name, image: base64Image, file});
				} catch (error) {
					logger.error(`Failed to prepare emoji ${file.name}`, error);
					preparationFailures.push({
						name: file.name,
						error: error instanceof Error ? error.message : String(error),
					});
				}
			}

			if (preparedEmojis.length === 0) {
				ModalActionCreators.pop();
				ModalActionCreators.push(
					modal(() => (
						<ConfirmModal
							title={t`Failed to Prepare Emojis`}
							description={
								<div className={styles.modalErrorContainer}>
									<p className={styles.modalErrorIntro}>
										<Trans>Unable to prepare any emojis for upload. The following errors occurred:</Trans>
									</p>
									{preparationFailures.map((failed, index) => (
										<div key={index} className={styles.modalErrorItem}>
											<div className={styles.modalErrorDetails}>
												<div className={styles.modalErrorName}>{failed.name}</div>
												<div className={styles.modalErrorMessage}>{failed.error}</div>
											</div>
										</div>
									))}
								</div>
							}
							primaryText={t`OK`}
							primaryVariant="primary"
							onPrimary={() => {}}
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
			for (const {chunk, startIndex} of chunkedPreparedEmojis) {
				try {
					const emojisToUpload = chunk.map(({name, image}) => ({name, image}));
					await GuildEmojiActionCreators.bulkUpload(guildId, emojisToUpload);
				} catch (error: unknown) {
					hasUploadError = true;
					logger.error(`Failed to upload emojis starting at index ${startIndex}`, error);
					const validationErrors = getApiErrorErrors(error);
					const errorMsg =
						validationErrors?.map((e) => e.message).join(', ') ??
						(error instanceof Error ? error.message : null) ??
						t`Failed to upload emojis. Please try again.`;
					ToastActionCreators.createToast({
						type: 'error',
						children: (
							<Trans>
								Failed to upload emojis starting at #{startIndex + 1}: {errorMsg}
							</Trans>
						),
					});
				}
			}

			if (!hasUploadError) {
				await fetchEmojis();
			}

			ModalActionCreators.pop();
		},
		[guildId, filteredEmojis, fetchEmojis, maxStaticEmojis],
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
		<div className={styles.container}>
			<div className={styles.controls}>
				<Input
					type="text"
					placeholder={t`Search emojis...`}
					value={searchQuery}
					onChange={(e) => setSearchQuery(e.target.value)}
					leftIcon={<MagnifyingGlassIcon size={16} weight="bold" />}
					className={styles.searchInput}
				/>
			</div>

			{fetchStatus === 'error' && (
				<div className={styles.notice}>
					<p className={styles.noticeText}>
						<WarningCircleIcon size={32} weight="fill" />
						<Trans>Failed to load emojis. Please try again later.</Trans>
					</p>
				</div>
			)}

			{canCreateExpressions && (
				<>
					<UploadSlotInfo
						title={<Trans>Emoji Slots</Trans>}
						currentCount={staticEmojis.length}
						maxCount={maxStaticEmojis}
						uploadButtonText={<Trans>Upload Emoji</Trans>}
						onUploadClick={async () => {
							const files = await openFilePicker({
								multiple: true,
								accept: '.jpg,.jpeg,.png,.apng,.gif,.webp,.avif,image/*',
							});
							if (files.length > 0) {
								void handleFileSelect(files);
							}
						}}
						description={
							<Trans>
								Emoji names must be at least 2 characters long and can only contain alphanumeric characters and
								underscores. Allowed file types: JPEG, PNG, WebP, GIF. We compress images to 128x128 pixels. Maximum
								size: {Math.round(GlobalLimits.getEmojiMaxSize() / 1024)} KB per emoji.
							</Trans>
						}
						additionalSlots={
							<>
								<span>
									<Trans>
										Static: {staticEmojis.length} /{' '}
										{maxStaticEmojis === Number.POSITIVE_INFINITY ? '∞' : maxStaticEmojis}
									</Trans>
								</span>
								<span>
									<Trans>
										Animated: {animatedEmojis.length} /{' '}
										{maxAnimatedEmojis === Number.POSITIVE_INFINITY ? '∞' : maxAnimatedEmojis}
									</Trans>
								</span>
							</>
						}
					/>

					<UploadDropZone onDrop={handleDrop} description={<Trans>Drag and drop emoji files here</Trans>} />
				</>
			)}

			{searchQuery && filteredEmojis.length === 0 && (
				<div className={styles.notice}>
					<p className={styles.noticeText}>
						<Trans>No emojis found matching your search.</Trans>
					</p>
				</div>
			)}

			{fetchStatus === 'pending' && (
				<div className={styles.spinnerContainer}>
					<Spinner />
				</div>
			)}

			{fetchStatus === 'success' && (staticEmojis.length > 0 || animatedEmojis.length > 0) && (
				<div className={clsx(styles.emojiSections, layout === 'grid' && styles.emojiSectionsGrid)}>
					{staticEmojis.length > 0 && (
						<div className={styles.emojiSection}>
							<h3 className={styles.emojiSectionTitle}>
								<Trans>Non-Animated Emoji ({staticEmojis.length})</Trans>
							</h3>
							{layout === 'list' && <EmojiListHeader />}
							<div className={layout === 'list' ? styles.emojiItemsList : styles.emojiGrid}>
								{staticEmojis.map((emoji: GuildEmojiWithUser) => (
									<EmojiListItem
										key={emoji.id}
										guildId={guildId}
										emoji={emoji}
										layout={layout}
										canModify={canModifyEmoji(emoji)}
										onRename={handleEmojiRename}
										onRemove={handleEmojiDelete}
									/>
								))}
							</div>
						</div>
					)}
					{animatedEmojis.length > 0 && (
						<div className={styles.emojiSection}>
							<h3 className={styles.emojiSectionTitle}>
								<Trans>Animated Emoji ({animatedEmojis.length})</Trans>
							</h3>
							{layout === 'list' && <EmojiListHeader />}
							<div className={layout === 'list' ? styles.emojiItemsList : styles.emojiGrid}>
								{animatedEmojis.map((emoji: GuildEmojiWithUser) => (
									<EmojiListItem
										key={emoji.id}
										guildId={guildId}
										emoji={emoji}
										layout={layout}
										canModify={canModifyEmoji(emoji)}
										onRename={handleEmojiRename}
										onRemove={handleEmojiDelete}
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
