// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import styles from '@app/features/channel/components/modals/EditGroupModal.module.css';
import {GroupMatureContentSwitch} from '@app/features/channel/components/modals/GroupMatureContentSwitch';
import {useEditGroupForm} from '@app/features/channel/components/modals/useEditGroupForm';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {EDIT_GROUP_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {PlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {Controller} from 'react-hook-form';

const ICON_FILE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR = msg({
	message: 'Icon file is too large. Choose a file smaller than {imageMaxSizeLabel}.',
	comment: 'Error message in the edit group modal. Preserve {imageMaxSizeLabel}; it is inserted by code.',
});
const ANIMATED_ICONS_ARE_NOT_SUPPORTED_PLEASE_USE_DESCRIPTOR = msg({
	message: 'Animated icons are not supported. Use a static image.',
	comment: 'Description text in the edit group modal.',
});
const GROUP_NAME_MUST_NOT_EXCEED_100_CHARACTERS_DESCRIPTOR = msg({
	message: 'Group name must not exceed 100 characters',
	comment: 'Label in the edit group modal.',
});
const GROUP_NAME_DESCRIPTOR = msg({
	message: 'Group name',
	comment: 'Short label in the edit group modal. Keep it concise.',
});
const MY_GROUP_DESCRIPTOR = msg({
	message: 'My group',
	comment: 'Short label in the edit group modal. Keep it concise.',
});
const CHANGE_ICON_DESCRIPTOR = msg({
	message: 'Change icon',
	comment:
		'Title of the modal where the user picks a group icon source. Keep it concise. Keep the tone plain and specific.',
});

const ICON_FILE_IS_TOO_LARGE_TITLE_DESCRIPTOR = msg({
	message: 'Icon file is too large',
	comment: 'Title of the error modal shown when the selected group icon exceeds the size limit.',
});
const UNSUPPORTED_ICON_FORMAT_DESCRIPTOR = msg({
	message: 'Unsupported icon format',
	comment: 'Title of the error modal shown when the selected group icon format is unsupported.',
});
const ANIMATED_ICONS_ARE_NOT_SUPPORTED_TITLE_DESCRIPTOR = msg({
	message: 'Animated icons are not supported',
	comment: 'Title of the error modal shown when an animated group icon is selected.',
});
const COULDN_T_PROCESS_IMAGE_DESCRIPTOR = msg({
	message: "Couldn't process image",
	comment: 'Title of the error modal shown when a cropped group icon cannot be processed.',
});
const INVALID_IMAGE_DESCRIPTOR = msg({
	message: 'Invalid image',
	comment: 'Title of the error modal shown when the selected group icon image cannot be used.',
});
const EDIT_GROUP_FORM_MESSAGES = {
	iconFileTooLargeTitle: ICON_FILE_IS_TOO_LARGE_TITLE_DESCRIPTOR,
	unsupportedIconFormatTitle: UNSUPPORTED_ICON_FORMAT_DESCRIPTOR,
	animatedIconsNotSupportedTitle: ANIMATED_ICONS_ARE_NOT_SUPPORTED_TITLE_DESCRIPTOR,
	couldNotProcessImageTitle: COULDN_T_PROCESS_IMAGE_DESCRIPTOR,
	invalidImageTitle: INVALID_IMAGE_DESCRIPTOR,
	iconFileTooLarge: ICON_FILE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR,
	animatedIconsNotSupported: ANIMATED_ICONS_ARE_NOT_SUPPORTED_PLEASE_USE_DESCRIPTOR,
	changeIcon: CHANGE_ICON_DESCRIPTOR,
};

export const EditGroupModal = observer(({channelId}: {channelId: string}) => {
	const {i18n} = useLingui();
	const {
		channel,
		form,
		isOwner,
		previewIconUrl,
		iconPresentable,
		handleOpenIconUpload,
		handleClearIcon,
		handleSubmit,
		isSubmitting,
	} = useEditGroupForm({
		channelId,
		onDone: ModalCommands.pop,
		dataFlx: 'channel.edit-group-modal',
		messages: EDIT_GROUP_FORM_MESSAGES,
	});
	if (!channel) {
		return null;
	}
	const placeholderName = channel ? ChannelUtils.getDMDisplayName(channel) : '';
	return (
		<Modal.Root size="small" centered data-flx="channel.edit-group-modal.modal-root">
			<Form form={form} onSubmit={handleSubmit} data-flx="channel.edit-group-modal.form.submit">
				<Modal.Header title={i18n._(EDIT_GROUP_DESCRIPTOR)} data-flx="channel.edit-group-modal.modal-header" />
				<Modal.Content data-flx="channel.edit-group-modal.modal-content">
					<Modal.ContentLayout data-flx="channel.edit-group-modal.modal-content-layout">
						<div className={styles.iconSection} data-flx="channel.edit-group-modal.icon-section">
							<div className={styles.iconLabel} data-flx="channel.edit-group-modal.icon-label">
								<Trans>Group icon</Trans>
							</div>
							<div className={styles.iconContainer} data-flx="channel.edit-group-modal.icon-container">
								{previewIconUrl ? (
									<div
										className={styles.iconPreview}
										style={{
											backgroundImage: `url(${previewIconUrl})`,
										}}
										data-flx="channel.edit-group-modal.icon-preview"
									/>
								) : iconPresentable ? (
									<div
										className={styles.iconPreview}
										style={{
											backgroundImage: `url(${iconPresentable})`,
										}}
										data-flx="channel.edit-group-modal.icon-preview--2"
									/>
								) : (
									<div className={styles.iconPlaceholder} data-flx="channel.edit-group-modal.icon-placeholder">
										<PlusIcon
											weight="regular"
											className={styles.iconPlaceholderIcon}
											data-flx="channel.edit-group-modal.icon-placeholder-icon"
										/>
									</div>
								)}
								<div className={styles.iconActions} data-flx="channel.edit-group-modal.icon-actions">
									<div className={styles.iconButtonGroup} data-flx="channel.edit-group-modal.icon-button-group">
										<Button
											variant="secondary"
											small={true}
											onClick={handleOpenIconUpload}
											data-flx="channel.edit-group-modal.button.icon-upload-click"
										>
											{previewIconUrl || iconPresentable ? <Trans>Change icon</Trans> : <Trans>Upload icon</Trans>}
										</Button>
										{(previewIconUrl || iconPresentable) && (
											<Button
												variant="secondary"
												small={true}
												onClick={handleClearIcon}
												data-flx="channel.edit-group-modal.button.clear-icon"
											>
												<Trans>Remove icon</Trans>
											</Button>
										)}
									</div>
								</div>
							</div>
							{form.formState.errors.icon?.message && (
								<p className={styles.iconError} data-flx="channel.edit-group-modal.icon-error">
									{form.formState.errors.icon.message}
								</p>
							)}
						</div>
						<Input
							data-flx="channel.edit-group-modal.input.text"
							{...form.register('name', {
								maxLength: {
									value: 100,
									message: i18n._(GROUP_NAME_MUST_NOT_EXCEED_100_CHARACTERS_DESCRIPTOR),
								},
							})}
							type="text"
							label={i18n._(GROUP_NAME_DESCRIPTOR)}
							placeholder={placeholderName || i18n._(MY_GROUP_DESCRIPTOR)}
							maxLength={100}
							error={form.formState.errors.name?.message}
						/>
						{isOwner && (
							<Controller
								name="nsfw"
								control={form.control}
								render={({field}) => (
									<GroupMatureContentSwitch
										value={field.value}
										onChange={field.onChange}
										data-flx="channel.edit-group-modal.group-mature-content-switch.change"
									/>
								)}
								data-flx="channel.edit-group-modal.controller"
							/>
						)}
					</Modal.ContentLayout>
				</Modal.Content>
				<Modal.Footer data-flx="channel.edit-group-modal.modal-footer">
					<Button type="submit" submitting={isSubmitting} data-flx="channel.edit-group-modal.button.submit">
						<Trans>Save</Trans>
					</Button>
				</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});
