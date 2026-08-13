// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {useCursorAtEnd} from '@app/features/app/hooks/useCursorAtEnd';
import Guilds from '@app/features/guild/state/Guilds';
import {CANCEL_DESCRIPTOR, FOLDER_SETTINGS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {ColorPickerField} from '@app/features/ui/components/form/ColorPickerField';
import {Combobox, type ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import * as UserSettingsCommands from '@app/features/user/commands/UserSettingsCommands';
import UserSettings from '@app/features/user/state/UserSettings';
import * as FormUtils from '@app/lib/forms';
import {
	DEFAULT_GUILD_FOLDER_ICON,
	GuildFolderFlags,
	type GuildFolderIcon,
	GuildFolderIcons,
	UNCATEGORIZED_FOLDER_ID,
} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {
	BookmarkSimpleIcon,
	FolderIcon,
	GameControllerIcon,
	HeartIcon,
	MusicNoteIcon,
	ShieldIcon,
	StarIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type {ReactNode} from 'react';
import {useCallback, useMemo, useState} from 'react';

const FOLDER_DESCRIPTOR = msg({
	message: 'Folder',
	comment: 'Folder icon option in the community-folder settings modal: a generic folder shape. Short standalone label.',
});
const STAR_DESCRIPTOR = msg({
	message: 'Star',
	comment: 'Folder icon option in the community-folder settings modal: a star shape. Short standalone label.',
});
const HEART_DESCRIPTOR = msg({
	message: 'Heart',
	comment: 'Folder icon option in the community-folder settings modal: a heart shape. Short standalone label.',
});
const BOOKMARK_DESCRIPTOR = msg({
	message: 'Bookmark',
	comment:
		'Folder icon option in the community-folder settings modal: a bookmark shape. Refers to the bookmark icon glyph, not the saved-message bookmarks feature.',
});
const GAME_CONTROLLER_DESCRIPTOR = msg({
	message: 'Game controller',
	comment: 'Folder icon option in the community-folder settings modal: a game controller shape.',
});
const SHIELD_DESCRIPTOR = msg({
	message: 'Shield',
	comment: 'Folder icon option in the community-folder settings modal: a shield shape. Short standalone label.',
});
const MUSIC_NOTE_DESCRIPTOR = msg({
	message: 'Music note',
	comment: 'Folder icon option in the community-folder settings modal: a music note shape.',
});
const FOLDER_NAME_DESCRIPTOR = msg({
	message: 'Folder name',
	comment: 'Label of the folder name input in the community-folder settings modal.',
});
const FOLDER_COLOR_DESCRIPTOR = msg({
	message: 'Folder color',
	comment: 'Label of the folder color picker in the community-folder settings modal.',
});
const SHOW_ICON_WHEN_COLLAPSED_DESCRIPTOR = msg({
	message: 'Show icon when collapsed',
	comment:
		'Switch label in the community-folder settings modal. When on, the folder icon is shown in the sidebar while the folder is collapsed.',
});
const FOLDER_ICON_DESCRIPTOR = msg({
	message: 'Folder icon',
	comment: 'Label of the folder icon dropdown in the community-folder settings modal.',
});
const DELETE_FOLDER_DESCRIPTOR = msg({
	message: 'Delete folder',
	comment:
		'Destructive button in the community-folder settings modal footer that removes the folder (communities inside are uncategorized).',
});
const SAVE_DESCRIPTOR = msg({
	message: 'Save',
	comment: 'Save button in the community-folder settings modal footer.',
});
const FOLDER_ICON_MAP: Record<GuildFolderIcon, ReactNode> = {
	[GuildFolderIcons.FOLDER]: (
		<FolderIcon weight="fill" size={remFromPx(18)} data-flx="guild.guild-folder-settings-modal.folder-icon" />
	),
	[GuildFolderIcons.STAR]: (
		<StarIcon weight="fill" size={remFromPx(18)} data-flx="guild.guild-folder-settings-modal.star-icon" />
	),
	[GuildFolderIcons.HEART]: (
		<HeartIcon weight="fill" size={remFromPx(18)} data-flx="guild.guild-folder-settings-modal.heart-icon" />
	),
	[GuildFolderIcons.BOOKMARK]: (
		<BookmarkSimpleIcon
			weight="fill"
			size={remFromPx(18)}
			data-flx="guild.guild-folder-settings-modal.bookmark-simple-icon"
		/>
	),
	[GuildFolderIcons.GAME_CONTROLLER]: (
		<GameControllerIcon
			weight="fill"
			size={remFromPx(18)}
			data-flx="guild.guild-folder-settings-modal.game-controller-icon"
		/>
	),
	[GuildFolderIcons.SHIELD]: (
		<ShieldIcon weight="fill" size={remFromPx(18)} data-flx="guild.guild-folder-settings-modal.shield-icon" />
	),
	[GuildFolderIcons.MUSIC_NOTE]: (
		<MusicNoteIcon weight="fill" size={remFromPx(18)} data-flx="guild.guild-folder-settings-modal.music-note-icon" />
	),
};

interface GuildFolderSettingsModalProps {
	folderId: number;
}

export const GuildFolderSettingsModal = observer(({folderId}: GuildFolderSettingsModalProps) => {
	const {i18n} = useLingui();
	const folder = useMemo(() => {
		return UserSettings.guildFolders.find((f) => f.id === folderId);
	}, [folderId]);
	const autoGeneratedName = useMemo(() => {
		if (!folder) return '';
		const guildNames = folder.guildIds
			.slice(0, 3)
			.map((guildId) => Guilds.getGuild(guildId)?.name)
			.filter((name): name is string => name != null);
		return guildNames.join(', ');
	}, [folder]);
	const [name, setName] = useState(folder?.name ?? '');
	const nameRef = useCursorAtEnd<HTMLInputElement>();
	const [color, setColor] = useState(folder?.color ?? 0);
	const [flags, setFlags] = useState(folder?.flags ?? 0);
	const [icon, setIcon] = useState<GuildFolderIcon>(folder?.icon ?? DEFAULT_GUILD_FOLDER_ICON);
	const [isSaving, setIsSaving] = useState(false);
	const [isDeleting, setIsDeleting] = useState(false);
	const [nameError, setNameError] = useState<string | null>(null);
	const showCollapsedIcon =
		(flags & GuildFolderFlags.SHOW_ICON_WHEN_COLLAPSED) === GuildFolderFlags.SHOW_ICON_WHEN_COLLAPSED;
	const iconOptions = useMemo<Array<ComboboxOption<GuildFolderIcon>>>(
		() => [
			{value: GuildFolderIcons.FOLDER, label: i18n._(FOLDER_DESCRIPTOR)},
			{value: GuildFolderIcons.STAR, label: i18n._(STAR_DESCRIPTOR)},
			{value: GuildFolderIcons.HEART, label: i18n._(HEART_DESCRIPTOR)},
			{value: GuildFolderIcons.BOOKMARK, label: i18n._(BOOKMARK_DESCRIPTOR)},
			{value: GuildFolderIcons.GAME_CONTROLLER, label: i18n._(GAME_CONTROLLER_DESCRIPTOR)},
			{value: GuildFolderIcons.SHIELD, label: i18n._(SHIELD_DESCRIPTOR)},
			{value: GuildFolderIcons.MUSIC_NOTE, label: i18n._(MUSIC_NOTE_DESCRIPTOR)},
		],
		[i18n.locale],
	);
	const handleNameChange = useCallback((event: React.ChangeEvent<HTMLInputElement>) => {
		setName(event.target.value);
		setNameError(null);
	}, []);
	const handleColorChange = useCallback((newColor: number) => {
		setColor(newColor);
	}, []);
	const handleShowCollapsedIconChange = useCallback((value: boolean) => {
		setFlags((currentFlags) => {
			if (value) {
				return currentFlags | GuildFolderFlags.SHOW_ICON_WHEN_COLLAPSED;
			}
			return currentFlags & ~GuildFolderFlags.SHOW_ICON_WHEN_COLLAPSED;
		});
	}, []);
	const handleIconChange = useCallback((value: GuildFolderIcon) => {
		setIcon(value);
	}, []);
	const renderIconOption = useCallback(
		(option: ComboboxOption<GuildFolderIcon>, _isSelected: boolean) => (
			<span
				style={{display: 'flex', alignItems: 'center', gap: remFromPx(8)}}
				data-flx="guild.guild-folder-settings-modal.render-icon-option.span"
			>
				{FOLDER_ICON_MAP[option.value]}
				{option.label}
			</span>
		),
		[],
	);
	const handleCancel = useCallback(() => {
		ModalCommands.pop();
	}, []);
	const handleSave = useCallback(async () => {
		if (!folder) return;
		setIsSaving(true);
		setNameError(null);
		try {
			const updatedFolders = UserSettings.guildFolders.map((f) => {
				if (f.id === folderId) {
					return {
						...f,
						name: name.trim() || null,
						color: color || null,
						flags,
						icon,
					};
				}
				return f;
			});
			await UserSettingsCommands.update({guildFolders: updatedFolders});
			ModalCommands.pop();
		} catch (error) {
			setNameError(FormUtils.extractErrorMessage(i18n, error));
		} finally {
			setIsSaving(false);
		}
	}, [folder, folderId, name, color, flags, icon, i18n]);
	const handleDelete = useCallback(async () => {
		if (!folder) return;
		setIsDeleting(true);
		try {
			const updatedFolders = UserSettings.guildFolders.map((f) => {
				if (f.id === folderId) {
					return {
						id: UNCATEGORIZED_FOLDER_ID,
						name: null,
						color: null,
						flags: 0,
						icon: DEFAULT_GUILD_FOLDER_ICON,
						guildIds: f.guildIds,
					};
				}
				return f;
			});
			await UserSettingsCommands.update({guildFolders: updatedFolders});
			ModalCommands.pop();
		} finally {
			setIsDeleting(false);
		}
	}, [folder, folderId]);
	if (!folder) {
		return null;
	}
	return (
		<Modal.Root size="small" centered data-flx="guild.guild-folder-settings-modal.modal-root">
			<Modal.Header
				title={i18n._(FOLDER_SETTINGS_DESCRIPTOR)}
				data-flx="guild.guild-folder-settings-modal.modal-header"
			/>
			<Modal.Content data-flx="guild.guild-folder-settings-modal.modal-content">
				<Modal.ContentLayout data-flx="guild.guild-folder-settings-modal.modal-content-layout">
					<Input
						ref={nameRef}
						autoFocus={true}
						label={i18n._(FOLDER_NAME_DESCRIPTOR)}
						placeholder={autoGeneratedName}
						value={name}
						onChange={handleNameChange}
						autoComplete="off"
						maxLength={100}
						error={nameError ?? undefined}
						data-flx="guild.guild-folder-settings-modal.input.name-change"
					/>
					<ColorPickerField
						label={i18n._(FOLDER_COLOR_DESCRIPTOR)}
						value={color}
						onChange={handleColorChange}
						data-flx="guild.guild-folder-settings-modal.color-picker-field.color-change"
					/>
					<Switch
						label={i18n._(SHOW_ICON_WHEN_COLLAPSED_DESCRIPTOR)}
						value={showCollapsedIcon}
						onChange={handleShowCollapsedIconChange}
						data-flx="guild.guild-folder-settings-modal.switch.show-collapsed-icon-change"
					/>
					<Combobox
						label={i18n._(FOLDER_ICON_DESCRIPTOR)}
						value={icon}
						options={iconOptions}
						onChange={handleIconChange}
						isSearchable={false}
						renderOption={renderIconOption}
						data-flx="guild.guild-folder-settings-modal.select.icon-change"
					/>
				</Modal.ContentLayout>
			</Modal.Content>
			<Modal.Footer data-flx="guild.guild-folder-settings-modal.modal-footer">
				<Button
					onClick={handleDelete}
					submitting={isDeleting}
					variant="danger"
					data-flx="guild.guild-folder-settings-modal.button.delete"
				>
					{i18n._(DELETE_FOLDER_DESCRIPTOR)}
				</Button>
				<Button onClick={handleCancel} variant="secondary" data-flx="guild.guild-folder-settings-modal.button.cancel">
					{i18n._(CANCEL_DESCRIPTOR)}
				</Button>
				<Button onClick={handleSave} submitting={isSaving} data-flx="guild.guild-folder-settings-modal.button.save">
					{i18n._(SAVE_DESCRIPTOR)}
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});

export function openGuildFolderSettingsModal(folderId: number): void {
	ModalCommands.push(
		ModalCommands.modal(() => (
			<GuildFolderSettingsModal
				folderId={folderId}
				data-flx="guild.guild-folder-settings-modal.open-guild-folder-settings-modal.guild-folder-settings-modal"
			/>
		)),
	);
}
