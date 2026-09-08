// SPDX-License-Identifier: AGPL-3.0-or-later

import {PREMIUM_PRODUCT_NAME, PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {LimitResolver} from '@app/features/app/utils/LimitResolverAdapter';
import {isLimitToggleEnabled} from '@app/features/app/utils/LimitUtils';
import Guilds from '@app/features/guild/state/Guilds';
import {DIRECT_MESSAGES_DESCRIPTOR, GET_PREMIUM_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import SelectedGuild from '@app/features/navigation/state/SelectedGuild';
import {
	DMS_ENTRANCE_SOUND_SCOPE,
	type EntranceSoundScope,
	GLOBAL_ENTRANCE_SOUND_SCOPE,
	GUILDS_ENTRANCE_SOUND_SCOPE,
	getEntranceSoundScopeId,
	parseEntranceSoundScopeId,
} from '@app/features/notification/utils/EntranceSoundScopes';
import * as PremiumModalCommands from '@app/features/premium/commands/PremiumModalCommands';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import {Button} from '@app/features/ui/button/Button';
import type {ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {CompactComboboxRow} from '@app/features/user/components/modals/tabs/components/CompactComboboxRow';
import styles from '@app/features/user/components/modals/tabs/components/EntranceSoundSection.module.css';
import {useEntranceSound} from '@app/features/user/components/modals/tabs/hooks/useEntranceSound';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import type {EntranceSoundEntry} from '@app/features/voice/state/EntranceSoundLibrary';
import {
	ENTRANCE_SOUND_MAX_BYTES,
	ENTRANCE_SOUND_MAX_DURATION_MS,
	ENTRANCE_SOUND_MAX_PER_USER,
} from '@fluxer/constants/src/EntranceSoundConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {
	ChatCircleTextIcon,
	CheckIcon,
	CrownIcon,
	GlobeHemisphereWestIcon,
	type Icon,
	SpeakerHighIcon,
	TrashIcon,
	UploadIcon,
	UsersThreeIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useId, useMemo, useState} from 'react';

const ALL_COMMUNITIES_AND_DMS_DESCRIPTOR = msg({
	message: 'All communities and DMs',
	comment:
		'Entrance sound scope label. Means the default entrance sound used across all communities and direct-message calls unless a more specific override exists.',
});
const ALL_COMMUNITIES_DESCRIPTOR = msg({
	message: 'All communities',
	comment: 'Short label in the entrance sound section. Keep it concise.',
});
const UNKNOWN_COMMUNITY_DESCRIPTOR = msg({
	message: 'Unknown community',
	comment: 'Short label in the entrance sound section. Keep it concise.',
});
const USED_BY_DEFAULT_UNLESS_A_COMMUNITY_OR_DM_DESCRIPTOR = msg({
	message: 'Used by default unless a community or DM sound overrides it.',
	comment: 'Description text in the entrance sound section.',
});
const USED_IN_COMMUNITIES_THAT_DO_NOT_HAVE_THEIR_DESCRIPTOR = msg({
	message: 'Used in communities that do not have their own sound.',
	comment: 'Description text in the entrance sound section.',
});
const USED_FOR_ALL_DIRECT_MESSAGE_CALLS_DESCRIPTOR = msg({
	message: 'Used for all direct-message calls.',
	comment: 'Description text in the entrance sound section.',
});
const CUSTOM_ENTRANCE_SOUNDS_PREMIUM_DESCRIPTOR = msg({
	message: 'Custom entrance sounds with {premiumProductName}',
	comment: 'Premium upsell title in entrance sound settings.',
});
const USED_ONLY_IN_THIS_COMMUNITY_DESCRIPTOR = msg({
	message: 'Used only in this community.',
	comment: 'Description text in the entrance sound section.',
});
const YOUR_FALLBACK_ENTRANCE_SOUND_FOR_ALL_COMMUNITIES_AND_DESCRIPTOR = msg({
	message: 'Your fallback entrance sound for all communities and direct messages. More specific settings override it.',
	comment: 'Description text in the entrance sound section.',
});
const USED_IN_COMMUNITIES_THAT_DO_NOT_HAVE_THEIR_2_DESCRIPTOR = msg({
	message: 'Used in communities that do not have their own sound set.',
	comment: 'Description text in the entrance sound section.',
});
const USED_IN_DIRECT_MESSAGE_CALLS_IF_YOU_DO_DESCRIPTOR = msg({
	message: 'Used in direct-message calls. If you do not set one, your global sound is used instead.',
	comment: 'Description text in the entrance sound section.',
});
const USED_IN_THIS_COMMUNITY_IF_YOU_DO_NOT_DESCRIPTOR = msg({
	message:
		'Used in this community. If you do not set one, {productName} falls back to your community default, then your global default.',
	comment:
		'Description text in the entrance sound section. Preserve {productName}; it is inserted by code and must appear verbatim in the translation.',
});
const UPLOAD_NEW_DESCRIPTOR = msg({
	message: 'Upload new',
	comment: 'Button label in the entrance sound section. Uploads a new sound into the user library.',
});
const SET_HERE_DESCRIPTOR = msg({
	message: 'Set here',
	comment:
		'Badge label in entrance sound settings. The current scope has its own explicitly configured sound instead of inheriting one.',
});
const INHERITED_DESCRIPTOR = msg({
	message: 'Inherited',
	comment: 'Short label in the entrance sound section. Keep it concise.',
});
const INHERITED_FROM_DESCRIPTOR = msg({
	message: 'Inherited from {inheritedFromLabel}',
	comment:
		'Short label in the entrance sound section. Keep it concise. Preserve {inheritedFromLabel}; it is inserted by code.',
});
const PREVIEW_SOUND_DESCRIPTOR = msg({
	message: 'Preview sound',
	comment: 'Short label in the entrance sound section. Keep it concise.',
});
const APPLIES_TO_DESCRIPTOR = msg({
	message: 'Applies to',
	comment: 'Short label in the entrance sound section. Keep it concise.',
});
const COMMUNITY_OVERRIDES_DESCRIPTOR = msg({
	message: 'Community overrides',
	comment: 'Label for the list of individual communities in entrance sound settings.',
});
const DELETE_FROM_LIBRARY_DESCRIPTOR = msg({
	message: 'Delete from library',
	comment: 'Button or menu action label in the entrance sound section. Removes the file from the user library.',
});
const ACTIVE_BADGE_DESCRIPTOR = msg({
	message: 'Active here',
	comment: 'Badge label shown on the library row that is currently selected for the chosen scope.',
});
const LIBRARY_COUNT_DESCRIPTOR = msg({
	message: 'Library: {count} of {max}',
	comment: 'Label showing how many sounds the user has saved relative to the maximum allowed.',
});
const NONE_LIBRARY_OPTION_DESCRIPTOR = msg({
	message: 'None',
	comment: 'Library picker entry meaning no sound is selected for this scope.',
});
const SOUND_FOR_THIS_SCOPE_DESCRIPTOR = msg({
	message: 'Sound for this scope',
	comment: 'Label for the dropdown that picks which library sound to use for the chosen scope.',
});

interface EntranceSoundScopeTileOption {
	value: string;
	label: string;
	description: string;
	icon?: Icon;
	guildId?: string;
	guildIcon?: string | null;
}

interface LibrarySoundOption extends ComboboxOption<string> {
	value: string;
	label: string;
	description: string;
}

const NONE_LIBRARY_VALUE = '__none__';

function formatEntranceSoundByteLimit(bytes: number): string {
	if (bytes >= 1024 * 1024) {
		const mb = bytes / (1024 * 1024);
		return `${mb % 1 === 0 ? mb.toFixed(0) : mb.toFixed(1)}MB`;
	}
	return `${Math.floor(bytes / 1024)}KB`;
}

interface ScopeArtworkProps {
	option: EntranceSoundScopeTileOption;
	selected: boolean;
	compact?: boolean;
}

function ScopeArtwork({option, selected, compact}: ScopeArtworkProps): React.ReactElement {
	if (option.guildId) {
		const iconUrl = option.guildIcon ? AvatarUtils.getGuildIconURL({id: option.guildId, icon: option.guildIcon}) : null;
		const initial = option.label.trim().charAt(0).toUpperCase() || '?';
		return iconUrl ? (
			<span
				className={clsx(styles.scopeAvatar, compact && styles.scopeAvatarCompact)}
				style={{backgroundImage: `url(${iconUrl})`}}
				aria-hidden
				data-flx="user.entrance-sound-section.scope-artwork.scope-avatar"
			/>
		) : (
			<span
				className={clsx(styles.scopeAvatarPlaceholder, compact && styles.scopeAvatarCompact)}
				aria-hidden
				data-flx="user.entrance-sound-section.scope-artwork.scope-avatar-placeholder"
			>
				{initial}
			</span>
		);
	}
	const IconComponent = option.icon ?? GlobeHemisphereWestIcon;
	return (
		<span
			className={clsx(styles.scopeIconWrap, selected && styles.scopeIconWrapSelected)}
			aria-hidden
			data-flx="user.entrance-sound-section.scope-artwork.scope-icon-wrap"
		>
			<IconComponent
				size={compact ? 16 : 18}
				weight={selected ? 'fill' : 'regular'}
				data-flx="user.entrance-sound-section.scope-artwork.icon"
			/>
		</span>
	);
}

interface ScopeTileButtonProps {
	option: EntranceSoundScopeTileOption;
	selected: boolean;
	onSelect: (value: string) => void;
	compact?: boolean;
	dataFlx: string;
}

function ScopeTileButton({option, selected, onSelect, compact, dataFlx}: ScopeTileButtonProps): React.ReactElement {
	return (
		<button
			type="button"
			className={clsx(styles.scopeTile, compact && styles.scopeTileCompact, selected && styles.scopeTileSelected)}
			aria-pressed={selected}
			aria-label={`${option.label}. ${option.description}`}
			onClick={() => onSelect(option.value)}
			data-flx={dataFlx}
		>
			<ScopeArtwork
				option={option}
				selected={selected}
				compact={compact}
				data-flx="user.entrance-sound-section.scope-tile-button.scope-artwork"
			/>
			<span className={styles.scopeTileText} data-flx={`${dataFlx}.text`}>
				<span className={styles.scopeTileLabel} data-flx={`${dataFlx}.label`}>
					{option.label}
				</span>
				{!compact ? (
					<span className={styles.scopeTileDescription} data-flx={`${dataFlx}.description`}>
						{option.description}
					</span>
				) : null}
			</span>
			{selected ? (
				<span className={styles.scopeTileCheck} aria-hidden data-flx={`${dataFlx}.check`}>
					<CheckIcon size={12} weight="bold" data-flx={`${dataFlx}.check-icon`} />
				</span>
			) : null}
		</button>
	);
}

export const EntranceSoundSection: React.FC = observer(() => {
	const scopePickerLabelId = useId();
	const hasVoiceEntranceSounds = useMemo(
		() =>
			isLimitToggleEnabled(
				{feature_voice_entrance_sounds: LimitResolver.resolve({key: 'feature_voice_entrance_sounds', fallback: 0})},
				'feature_voice_entrance_sounds',
			),
		[],
	);
	const {i18n} = useLingui();
	const maxDurationSeconds = (ENTRANCE_SOUND_MAX_DURATION_MS / 1000).toFixed(1);
	const maxSizeLabel = formatEntranceSoundByteLimit(ENTRANCE_SOUND_MAX_BYTES);
	const currentGuildId = SelectedGuild.selectedGuildId;
	const guilds = Guilds.getGuilds();
	const [selectedScopeId, setSelectedScopeId] = useState(() =>
		getEntranceSoundScopeId(currentGuildId ? {kind: 'guild', guildId: currentGuildId} : GLOBAL_ENTRANCE_SOUND_SCOPE),
	);
	const selectedScope = useMemo(
		() => parseEntranceSoundScopeId(selectedScopeId) ?? GLOBAL_ENTRANCE_SOUND_SCOPE,
		[selectedScopeId],
	);
	const orderedGuilds = useMemo(() => {
		const visibleGuilds = [...guilds];
		return visibleGuilds.sort((left, right) => {
			if (left.id === currentGuildId) return -1;
			if (right.id === currentGuildId) return 1;
			return (left.name || '').localeCompare(right.name || '');
		});
	}, [currentGuildId, guilds]);
	const getScopeLabel = useCallback(
		(scope: EntranceSoundScope): string => {
			switch (scope.kind) {
				case 'global':
					return i18n._(ALL_COMMUNITIES_AND_DMS_DESCRIPTOR);
				case 'guilds':
					return i18n._(ALL_COMMUNITIES_DESCRIPTOR);
				case 'dms':
					return i18n._(DIRECT_MESSAGES_DESCRIPTOR);
				case 'guild':
					return Guilds.getGuild(scope.guildId)?.name ?? i18n._(UNKNOWN_COMMUNITY_DESCRIPTOR);
			}
		},
		[i18n.locale],
	);
	const getScopeDescription = useCallback(
		(scope: EntranceSoundScope): string => {
			switch (scope.kind) {
				case 'global':
					return i18n._(USED_BY_DEFAULT_UNLESS_A_COMMUNITY_OR_DM_DESCRIPTOR);
				case 'guilds':
					return i18n._(USED_IN_COMMUNITIES_THAT_DO_NOT_HAVE_THEIR_DESCRIPTOR);
				case 'dms':
					return i18n._(USED_FOR_ALL_DIRECT_MESSAGE_CALLS_DESCRIPTOR);
				case 'guild':
					return i18n._(USED_ONLY_IN_THIS_COMMUNITY_DESCRIPTOR);
			}
		},
		[i18n.locale],
	);
	const currentGuild = useMemo(
		() => (currentGuildId ? (orderedGuilds.find((guild) => guild.id === currentGuildId) ?? null) : null),
		[currentGuildId, orderedGuilds],
	);
	const scopeTileOptions = useMemo<Array<EntranceSoundScopeTileOption>>(() => {
		const baseOptions: Array<EntranceSoundScopeTileOption> = [
			{
				value: getEntranceSoundScopeId(GLOBAL_ENTRANCE_SOUND_SCOPE),
				label: getScopeLabel(GLOBAL_ENTRANCE_SOUND_SCOPE),
				description: getScopeDescription(GLOBAL_ENTRANCE_SOUND_SCOPE),
				icon: GlobeHemisphereWestIcon,
			},
			{
				value: getEntranceSoundScopeId(GUILDS_ENTRANCE_SOUND_SCOPE),
				label: getScopeLabel(GUILDS_ENTRANCE_SOUND_SCOPE),
				description: getScopeDescription(GUILDS_ENTRANCE_SOUND_SCOPE),
				icon: UsersThreeIcon,
			},
			{
				value: getEntranceSoundScopeId(DMS_ENTRANCE_SOUND_SCOPE),
				label: getScopeLabel(DMS_ENTRANCE_SOUND_SCOPE),
				description: getScopeDescription(DMS_ENTRANCE_SOUND_SCOPE),
				icon: ChatCircleTextIcon,
			},
		];
		if (currentGuild) {
			baseOptions.push({
				value: getEntranceSoundScopeId({kind: 'guild', guildId: currentGuild.id}),
				label: getScopeLabel({kind: 'guild', guildId: currentGuild.id}),
				description: getScopeDescription({kind: 'guild', guildId: currentGuild.id}),
				guildId: currentGuild.id,
				guildIcon: currentGuild.icon ?? null,
			});
		}
		return baseOptions;
	}, [currentGuild, getScopeDescription, getScopeLabel]);
	const communityScopeOptions = useMemo<Array<EntranceSoundScopeTileOption>>(
		() =>
			orderedGuilds
				.filter((guild) => guild.id !== currentGuild?.id)
				.map((guild) => ({
					value: getEntranceSoundScopeId({kind: 'guild', guildId: guild.id}),
					label: guild.name || i18n._(UNKNOWN_COMMUNITY_DESCRIPTOR),
					description: getScopeDescription({kind: 'guild', guildId: guild.id}),
					guildId: guild.id,
					guildIcon: guild.icon ?? null,
				})),
		[currentGuild?.id, getScopeDescription, i18n.locale, orderedGuilds],
	);
	const getScopeHint = useCallback(
		(scope: EntranceSoundScope): string => {
			switch (scope.kind) {
				case 'global':
					return i18n._(YOUR_FALLBACK_ENTRANCE_SOUND_FOR_ALL_COMMUNITIES_AND_DESCRIPTOR);
				case 'guilds':
					return i18n._(USED_IN_COMMUNITIES_THAT_DO_NOT_HAVE_THEIR_2_DESCRIPTOR);
				case 'dms':
					return i18n._(USED_IN_DIRECT_MESSAGE_CALLS_IF_YOU_DO_DESCRIPTOR);
				case 'guild':
					return i18n._(USED_IN_THIS_COMMUNITY_IF_YOU_DO_NOT_DESCRIPTOR, {productName: PRODUCT_NAME});
			}
		},
		[i18n],
	);
	const {
		library,
		selectedSound,
		resolvedSound,
		inheritedFromScope,
		isPreviewing,
		setSoundForScope,
		openUploadDialog,
		deleteSound,
		previewSound,
	} = useEntranceSound(selectedScope);
	const scopeHint = getScopeHint(selectedScope);
	const inheritedFromLabel = inheritedFromScope ? getScopeLabel(inheritedFromScope) : null;
	const hasDirectOverride = selectedSound != null;
	const hasInheritedSound = resolvedSound != null && !hasDirectOverride && inheritedFromLabel != null;

	const librarySelectOptions = useMemo<Array<LibrarySoundOption>>(
		() => [
			{value: NONE_LIBRARY_VALUE, label: i18n._(NONE_LIBRARY_OPTION_DESCRIPTOR), description: ''},
			...library.map((entry: EntranceSoundEntry) => ({
				value: entry.id,
				label: entry.name,
				description: `${(entry.durationMs / 1000).toFixed(1)}s`,
			})),
		],
		[library, i18n.locale],
	);

	const handleLibrarySelectionChange = useCallback(
		(nextValue: string) => {
			void setSoundForScope(nextValue === NONE_LIBRARY_VALUE ? null : nextValue);
		},
		[setSoundForScope],
	);

	return (
		<div data-flx="user.entrance-sound-section.div">
			{!hasVoiceEntranceSounds ? (
				shouldShowPremiumFeatures() ? (
					<div className={styles.premiumCard} data-flx="user.entrance-sound-section.premium-card">
						<div className={styles.premiumCardHeader} data-flx="user.entrance-sound-section.premium-card-header">
							<CrownIcon
								weight="fill"
								size={18}
								className={styles.premiumCardIcon}
								data-flx="user.entrance-sound-section.premium-card-icon"
							/>
							<span className={styles.premiumCardTitle} data-flx="user.entrance-sound-section.premium-card-title">
								{i18n._(CUSTOM_ENTRANCE_SOUNDS_PREMIUM_DESCRIPTOR, {
									premiumProductName: PREMIUM_PRODUCT_NAME,
								})}
							</span>
						</div>
						<p
							className={styles.premiumCardDescription}
							data-flx="user.entrance-sound-section.premium-card-description"
						>
							<Trans>
								Upload a custom sound that plays automatically when you join a voice channel. Maximum duration:{' '}
								{maxDurationSeconds} seconds. Max file size: {maxSizeLabel}.
							</Trans>
						</p>
						<Button
							variant="secondary"
							small={true}
							onClick={() => PremiumModalCommands.open()}
							data-flx="user.entrance-sound-section.button.open"
						>
							{i18n._(GET_PREMIUM_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
						</Button>
					</div>
				) : (
					<p
						className={styles.premiumCardDescription}
						data-flx="user.entrance-sound-section.premium-card-description--2"
					>
						<Trans>Custom entrance sounds are not enabled on this instance.</Trans>
					</p>
				)
			) : (
				<div className={styles.content} data-flx="user.entrance-sound-section.content">
					<section
						className={styles.scopeSection}
						aria-labelledby={scopePickerLabelId}
						data-flx="user.entrance-sound-section.scope-section"
					>
						<div className={styles.scopeSectionHeader} data-flx="user.entrance-sound-section.scope-section-header">
							<div className={styles.scopeSectionText} data-flx="user.entrance-sound-section.scope-section-text">
								<div
									id={scopePickerLabelId}
									className={styles.scopeSectionLabel}
									data-flx="user.entrance-sound-section.scope-section-label"
								>
									{i18n._(APPLIES_TO_DESCRIPTOR)}
								</div>
								<p className={styles.scopeHint} data-flx="user.entrance-sound-section.scope-hint">
									{scopeHint}
								</p>
							</div>
						</div>
						<div
							className={styles.scopeTileGrid}
							role="group"
							aria-labelledby={scopePickerLabelId}
							data-flx="user.entrance-sound-section.scope-tile-grid"
						>
							{scopeTileOptions.map((option) => (
								<ScopeTileButton
									key={option.value}
									option={option}
									selected={selectedScopeId === option.value}
									onSelect={setSelectedScopeId}
									dataFlx="user.entrance-sound-section.scope-tile"
									data-flx="user.entrance-sound-section.scope-tile-button.set-selected-scope-id"
								/>
							))}
						</div>
						{communityScopeOptions.length > 0 ? (
							<div className={styles.communityScopePanel} data-flx="user.entrance-sound-section.community-scope-panel">
								<div
									className={styles.communityScopeHeader}
									data-flx="user.entrance-sound-section.community-scope-header"
								>
									<span
										className={styles.communityScopeTitle}
										data-flx="user.entrance-sound-section.community-scope-title"
									>
										{i18n._(COMMUNITY_OVERRIDES_DESCRIPTOR)}
									</span>
								</div>
								<div
									className={styles.communityScopeList}
									role="group"
									aria-label={i18n._(COMMUNITY_OVERRIDES_DESCRIPTOR)}
									data-flx="user.entrance-sound-section.community-scope-list"
								>
									{communityScopeOptions.map((option) => (
										<ScopeTileButton
											key={option.value}
											option={option}
											selected={selectedScopeId === option.value}
											onSelect={setSelectedScopeId}
											compact={true}
											dataFlx="user.entrance-sound-section.community-scope-tile"
											data-flx="user.entrance-sound-section.scope-tile-button.set-selected-scope-id--2"
										/>
									))}
								</div>
							</div>
						) : null}
					</section>
					<div className={styles.controlPanel} data-flx="user.entrance-sound-section.control-panel">
						<CompactComboboxRow<string, LibrarySoundOption>
							label={i18n._(SOUND_FOR_THIS_SCOPE_DESCRIPTOR)}
							value={selectedSound?.id ?? NONE_LIBRARY_VALUE}
							options={librarySelectOptions}
							onChange={handleLibrarySelectionChange}
							controlWidth="large"
							menuMinWidth={220}
							dataFlx="user.entrance-sound-section.select--library"
							data-flx="user.entrance-sound-section.compact-combobox-row.library-selection-change"
						/>
					</div>
					{resolvedSound ? (
						<div className={styles.soundCard} data-flx="user.entrance-sound-section.sound-card">
							<div className={styles.soundCardContent} data-flx="user.entrance-sound-section.sound-card-content">
								<div className={styles.soundCardMain} data-flx="user.entrance-sound-section.sound-card-main">
									<Tooltip text={i18n._(PREVIEW_SOUND_DESCRIPTOR)} data-flx="user.entrance-sound-section.tooltip">
										<button
											type="button"
											onClick={() => void previewSound(resolvedSound.id)}
											disabled={isPreviewing}
											className={styles.previewButton}
											aria-label={i18n._(PREVIEW_SOUND_DESCRIPTOR)}
											data-flx="user.entrance-sound-section.preview-button"
										>
											<SpeakerHighIcon
												size={20}
												weight="fill"
												className={styles.previewIcon}
												data-flx="user.entrance-sound-section.preview-icon"
											/>
										</button>
									</Tooltip>
									<div className={styles.soundInfo} data-flx="user.entrance-sound-section.sound-info">
										<span className={styles.soundFileName} data-flx="user.entrance-sound-section.sound-file-name">
											{resolvedSound.name}
										</span>
										<span className={styles.soundDuration} data-flx="user.entrance-sound-section.sound-duration">
											{(resolvedSound.durationMs / 1000).toFixed(1)}s
										</span>
									</div>
								</div>
								<div className={styles.soundStatus} data-flx="user.entrance-sound-section.sound-status">
									<span
										className={clsx(
											styles.scopeBadge,
											hasDirectOverride ? styles.scopeBadgeDirect : styles.scopeBadgeInherited,
										)}
										data-flx="user.entrance-sound-section.scope-badge"
									>
										{hasDirectOverride ? i18n._(SET_HERE_DESCRIPTOR) : i18n._(INHERITED_DESCRIPTOR)}
									</span>
									{hasInheritedSound && inheritedFromLabel ? (
										<span className={styles.inheritedText} data-flx="user.entrance-sound-section.inherited-text">
											{i18n._(INHERITED_FROM_DESCRIPTOR, {inheritedFromLabel})}
										</span>
									) : null}
								</div>
							</div>
						</div>
					) : (
						<div className={styles.emptyState} data-flx="user.entrance-sound-section.empty-state">
							<div className={styles.emptyStateTitle} data-flx="user.entrance-sound-section.empty-state-title">
								<Trans comment="Empty-state title in entrance sound settings. The selected scope does not have its own entrance sound yet.">
									No entrance sound set here
								</Trans>
							</div>
						</div>
					)}
					<div className={styles.actions} data-flx="user.entrance-sound-section.actions">
						<Button
							variant="primary"
							className={styles.actionButton}
							onClick={openUploadDialog}
							data-flx="user.entrance-sound-section.action-button.open-upload-dialog"
						>
							<div className={styles.uploadButtonContent} data-flx="user.entrance-sound-section.upload-button-content">
								<UploadIcon size={16} data-flx="user.entrance-sound-section.upload-icon" />
								<span data-flx="user.entrance-sound-section.span">{i18n._(UPLOAD_NEW_DESCRIPTOR)}</span>
							</div>
						</Button>
						<span className={styles.scopeHint} data-flx="user.entrance-sound-section.library-count">
							{i18n._(LIBRARY_COUNT_DESCRIPTOR, {count: library.length, max: ENTRANCE_SOUND_MAX_PER_USER})}
						</span>
					</div>
					{library.length > 0 ? (
						<div className={styles.libraryList} data-flx="user.entrance-sound-section.library">
							{library.map((entry) => {
								const isActiveHere = selectedSound?.id === entry.id;
								return (
									<div key={entry.id} className={styles.soundCard} data-flx="user.entrance-sound-section.library-row">
										<div className={styles.soundCardContent} data-flx="user.entrance-sound-section.sound-card-content">
											<div className={styles.soundCardMain} data-flx="user.entrance-sound-section.sound-card-main">
												<Tooltip
													text={i18n._(PREVIEW_SOUND_DESCRIPTOR)}
													data-flx="user.entrance-sound-section.library-tooltip"
												>
													<button
														type="button"
														onClick={() => void previewSound(entry.id)}
														disabled={isPreviewing}
														className={styles.previewButton}
														aria-label={i18n._(PREVIEW_SOUND_DESCRIPTOR)}
														data-flx="user.entrance-sound-section.library-preview"
													>
														<SpeakerHighIcon
															size={18}
															weight="fill"
															className={styles.previewIcon}
															data-flx="user.entrance-sound-section.preview-icon"
														/>
													</button>
												</Tooltip>
												<div className={styles.soundInfo} data-flx="user.entrance-sound-section.sound-info">
													<span className={styles.soundFileName} data-flx="user.entrance-sound-section.sound-file-name">
														{entry.name}
													</span>
													<span className={styles.soundDuration} data-flx="user.entrance-sound-section.sound-duration">
														{`${(entry.durationMs / 1000).toFixed(1)}s${isActiveHere ? ` · ${i18n._(ACTIVE_BADGE_DESCRIPTOR)}` : ''}`}
													</span>
												</div>
											</div>
											<Tooltip
												text={i18n._(DELETE_FROM_LIBRARY_DESCRIPTOR)}
												data-flx="user.entrance-sound-section.library-delete-tooltip"
											>
												<button
													type="button"
													onClick={() => void deleteSound(entry.id)}
													className={styles.deleteButton}
													aria-label={i18n._(DELETE_FROM_LIBRARY_DESCRIPTOR)}
													data-flx="user.entrance-sound-section.library-delete"
												>
													<TrashIcon
														size={16}
														className={styles.deleteIcon}
														data-flx="user.entrance-sound-section.delete-icon"
													/>
												</button>
											</Tooltip>
										</div>
									</div>
								);
							})}
						</div>
					) : null}
				</div>
			)}
		</div>
	);
});
