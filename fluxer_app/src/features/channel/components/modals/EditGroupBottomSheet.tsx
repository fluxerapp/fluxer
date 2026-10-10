// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/modals/EditGroupBottomSheet.module.css';
import {GroupMatureContentSwitch} from '@app/features/channel/components/modals/GroupMatureContentSwitch';
import {useEditGroupForm} from '@app/features/channel/components/modals/useEditGroupForm';
import {EDIT_GROUP_DESCRIPTOR, GO_BACK_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import {Button} from '@app/features/ui/button/Button';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Scroller} from '@app/features/ui/components/Scroller';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowLeftIcon, PlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {Controller} from 'react-hook-form';

const ICON_FILE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR = msg({
	message: 'Icon file is too large. Choose a file smaller than {imageMaxSizeLabel}.',
	comment:
		'Error modal body in the mobile edit group sheet when the chosen icon file exceeds the size limit. imageMaxSizeLabel is a localized size.',
});
const ANIMATED_ICONS_ARE_NOT_SUPPORTED_PLEASE_USE_DESCRIPTOR = msg({
	message: 'Animated icons are not supported. Use a static image.',
	comment: 'Error modal body in the mobile edit group sheet when an animated icon is chosen.',
});
const EDIT_GROUP_FORM_DESCRIPTOR = msg({
	message: 'Edit group form',
	comment: 'Accessible label for the edit group form region in the mobile bottom sheet.',
});
const GROUP_NAME_DESCRIPTOR = msg({
	message: 'Group name',
	comment: 'Field label for the group name input in the mobile edit group sheet.',
});
const MY_GROUP_DESCRIPTOR = msg({
	message: 'My group',
	comment: 'Placeholder text in the group name input in the mobile edit group sheet.',
});
const CHANGE_ICON_DESCRIPTOR = msg({
	message: 'Change icon',
	comment:
		'Title of the modal where the user picks a group icon source in the mobile edit group sheet. Keep it concise. Keep the tone plain and specific.',
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

interface EditGroupBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	channelId: string;
}

export const EditGroupBottomSheet: React.FC<EditGroupBottomSheetProps> = observer(({isOpen, onClose, channelId}) => {
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
		onDone: onClose,
		dataFlx: 'channel.edit-group-bottom-sheet',
		messages: EDIT_GROUP_FORM_MESSAGES,
	});
	if (!channel) {
		return null;
	}
	return (
		<BottomSheet
			isOpen={isOpen}
			onClose={onClose}
			snapPoints={[0, 1]}
			initialSnap={1}
			disablePadding={true}
			surface="primary"
			leadingAction={
				<button
					type="button"
					onClick={onClose}
					className={styles.backButton}
					aria-label={i18n._(GO_BACK_DESCRIPTOR)}
					data-flx="channel.edit-group-bottom-sheet.back-button.close"
				>
					<ArrowLeftIcon
						className={styles.backIcon}
						weight="bold"
						data-flx="channel.edit-group-bottom-sheet.back-icon"
					/>
				</button>
			}
			title={i18n._(EDIT_GROUP_DESCRIPTOR)}
			data-flx="channel.edit-group-bottom-sheet.bottom-sheet"
		>
			<div className={styles.container} data-flx="channel.edit-group-bottom-sheet.container">
				<Scroller
					className={styles.scroller}
					key="edit-group-bottom-sheet-scroller"
					data-flx="channel.edit-group-bottom-sheet.scroller"
				>
					<div className={styles.scrollContent} data-flx="channel.edit-group-bottom-sheet.scroll-content">
						<Form
							form={form}
							onSubmit={handleSubmit}
							className={styles.form}
							aria-label={i18n._(EDIT_GROUP_FORM_DESCRIPTOR)}
							data-flx="channel.edit-group-bottom-sheet.form.submit"
						>
							<div className={styles.iconSection} data-flx="channel.edit-group-bottom-sheet.icon-section">
								<div className={styles.iconLabel} data-flx="channel.edit-group-bottom-sheet.icon-label">
									<Trans>Group icon</Trans>
								</div>
								<div className={styles.iconContainer} data-flx="channel.edit-group-bottom-sheet.icon-container">
									{previewIconUrl ? (
										<div
											className={styles.iconPreview}
											style={{
												backgroundImage: `url(${previewIconUrl})`,
											}}
											data-flx="channel.edit-group-bottom-sheet.icon-preview"
										/>
									) : iconPresentable ? (
										<div
											className={styles.iconPreview}
											style={{
												backgroundImage: `url(${iconPresentable})`,
											}}
											data-flx="channel.edit-group-bottom-sheet.icon-preview--2"
										/>
									) : (
										<div className={styles.iconPlaceholder} data-flx="channel.edit-group-bottom-sheet.icon-placeholder">
											<PlusIcon
												weight="regular"
												className={styles.iconPlaceholderIcon}
												data-flx="channel.edit-group-bottom-sheet.icon-placeholder-icon"
											/>
										</div>
									)}
									<div className={styles.iconActions} data-flx="channel.edit-group-bottom-sheet.icon-actions">
										<div
											className={styles.iconButtonGroup}
											data-flx="channel.edit-group-bottom-sheet.icon-button-group"
										>
											<Button
												variant="secondary"
												small={true}
												onClick={handleOpenIconUpload}
												data-flx="channel.edit-group-bottom-sheet.button.icon-upload-click"
											>
												{previewIconUrl || iconPresentable ? <Trans>Change icon</Trans> : <Trans>Upload icon</Trans>}
											</Button>
											{(previewIconUrl || iconPresentable) && (
												<Button
													variant="secondary"
													small={true}
													onClick={handleClearIcon}
													data-flx="channel.edit-group-bottom-sheet.button.clear-icon"
												>
													<Trans>Remove icon</Trans>
												</Button>
											)}
										</div>
									</div>
								</div>
								{form.formState.errors.icon?.message && (
									<p className={styles.iconError} data-flx="channel.edit-group-bottom-sheet.icon-error">
										{form.formState.errors.icon.message}
									</p>
								)}
							</div>
							<Input
								data-flx="channel.edit-group-bottom-sheet.input.text"
								{...form.register('name')}
								type="text"
								label={i18n._(GROUP_NAME_DESCRIPTOR)}
								placeholder={i18n._(MY_GROUP_DESCRIPTOR)}
								minLength={1}
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
											data-flx="channel.edit-group-bottom-sheet.group-mature-content-switch.change"
										/>
									)}
									data-flx="channel.edit-group-bottom-sheet.controller"
								/>
							)}
							<div className={styles.footer} data-flx="channel.edit-group-bottom-sheet.footer">
								<Button
									type="submit"
									submitting={isSubmitting}
									className={styles.fullWidth}
									data-flx="channel.edit-group-bottom-sheet.full-width.submit"
								>
									<Trans>Save</Trans>
								</Button>
							</div>
						</Form>
					</div>
				</Scroller>
			</div>
		</BottomSheet>
	);
});
