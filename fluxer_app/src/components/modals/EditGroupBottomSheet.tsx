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

import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowLeftIcon, PlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {useForm} from 'react-hook-form';
import * as ChannelActionCreators from '~/actions/ChannelActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {Form} from '~/components/form/Form';
import {Input} from '~/components/form/Input';
import styles from '~/components/modals/EditGroupBottomSheet.module.css';
import {BottomSheet} from '~/components/uikit/BottomSheet/BottomSheet';
import {Button} from '~/components/uikit/Button/Button';
import {Scroller} from '~/components/uikit/Scroller';
import {useFormSubmit} from '~/hooks/useFormSubmit';
import ChannelStore from '~/stores/ChannelStore';
import * as AvatarUtils from '~/utils/AvatarUtils';
import {openFilePicker} from '~/utils/FilePickerUtils';
import {AssetCropModal, AssetType} from './AssetCropModal';

interface FormInputs {
	icon?: string | null;
	name: string;
}

interface EditGroupBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	channelId: string;
}

export const EditGroupBottomSheet: React.FC<EditGroupBottomSheetProps> = observer(({isOpen, onClose, channelId}) => {
	const {t} = useLingui();
	const channel = ChannelStore.getChannel(channelId);
	const [hasClearedIcon, setHasClearedIcon] = React.useState(false);
	const [previewIconUrl, setPreviewIconUrl] = React.useState<string | null>(null);
	const form = useForm<FormInputs>({
		defaultValues: React.useMemo(() => ({name: channel?.name || ''}), [channel]),
	});

	const handleIconUpload = React.useCallback(
		async (file: File | null) => {
			try {
				if (!file) return;

				if (file.size > 10 * 1024 * 1024) {
					ToastActionCreators.createToast({
						type: 'error',
						children: t`Icon file is too large. Please choose a file smaller than 10MB.`,
					});
					return;
				}

				if (file.type === 'image/gif') {
					ToastActionCreators.createToast({
						type: 'error',
						children: t`Animated icons are not supported. Please use JPEG, PNG, or WebP.`,
					});
					return;
				}

				const base64 = await AvatarUtils.fileToBase64(file);

				ModalActionCreators.push(
					modal(() => (
						<AssetCropModal
							assetType={AssetType.CHANNEL_ICON}
							imageUrl={base64}
							sourceMimeType={file.type}
							onCropComplete={(croppedBlob) => {
								const reader = new FileReader();
								reader.onload = () => {
									const croppedBase64 = reader.result as string;
									form.setValue('icon', croppedBase64);
									setPreviewIconUrl(croppedBase64);
									setHasClearedIcon(false);
									form.clearErrors('icon');
								};
								reader.onerror = () => {
									ToastActionCreators.createToast({
										type: 'error',
										children: t`Failed to process the cropped image. Please try again.`,
									});
								};
								reader.readAsDataURL(croppedBlob);
							}}
							onSkip={() => {
								form.setValue('icon', base64);
								setPreviewIconUrl(base64);
								setHasClearedIcon(false);
								form.clearErrors('icon');
							}}
						/>
					)),
				);
			} catch {
				ToastActionCreators.createToast({
					type: 'error',
					children: <Trans>That image is invalid. Please try another one.</Trans>,
				});
			}
		},
		[form],
	);

	const handleIconUploadClick = React.useCallback(async () => {
		const [file] = await openFilePicker({accept: 'image/jpeg,image/png,image/webp,image/gif'});
		await handleIconUpload(file ?? null);
	}, [handleIconUpload]);

	const handleClearIcon = React.useCallback(() => {
		form.setValue('icon', null);
		setPreviewIconUrl(null);
		setHasClearedIcon(true);
	}, [form]);

	const onSubmit = React.useCallback(
		async (data: FormInputs) => {
			const newChannel = await ChannelActionCreators.update(channelId, {
				icon: data.icon,
				name: data.name,
			});
			form.reset({name: newChannel.name});
			ToastActionCreators.createToast({type: 'success', children: <Trans>Group updated</Trans>});
			onClose();
		},
		[channelId, form, onClose],
	);

	const {handleSubmit, isSubmitting} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'name',
	});

	if (!channel) {
		return null;
	}

	const iconPresentable = hasClearedIcon
		? null
		: (previewIconUrl ?? AvatarUtils.getChannelIconURL({id: channel.id, icon: channel.icon}, 256));

	return (
		<BottomSheet
			isOpen={isOpen}
			onClose={onClose}
			snapPoints={[0, 1]}
			initialSnap={1}
			disablePadding={true}
			surface="primary"
			leadingAction={
				<button type="button" onClick={onClose} className={styles.backButton}>
					<ArrowLeftIcon className={styles.backIcon} weight="bold" />
				</button>
			}
			title={t`Edit Group`}
		>
			<div className={styles.container}>
				<Scroller className={styles.scroller} key="edit-group-bottom-sheet-scroller">
					<div className={styles.scrollContent}>
						<Form form={form} onSubmit={handleSubmit} className={styles.form} aria-label={t`Edit group form`}>
							<div className={styles.iconSection}>
								<div className={styles.iconLabel}>
									<Trans>Group Icon</Trans>
								</div>
								<div className={styles.iconContainer}>
									{previewIconUrl ? (
										<div
											className={styles.iconPreview}
											style={{
												backgroundImage: `url(${previewIconUrl})`,
											}}
										/>
									) : iconPresentable ? (
										<div
											className={styles.iconPreview}
											style={{
												backgroundImage: `url(${iconPresentable})`,
											}}
										/>
									) : (
										<div className={styles.iconPlaceholder}>
											<PlusIcon weight="regular" className={styles.iconPlaceholderIcon} />
										</div>
									)}
									<div className={styles.iconActions}>
										<div className={styles.iconButtonGroup}>
											<Button variant="secondary" small={true} onClick={handleIconUploadClick}>
												{previewIconUrl || iconPresentable ? <Trans>Change Icon</Trans> : <Trans>Upload Icon</Trans>}
											</Button>
											{(previewIconUrl || iconPresentable) && (
												<Button variant="secondary" small={true} onClick={handleClearIcon}>
													<Trans>Remove Icon</Trans>
												</Button>
											)}
										</div>
										<div className={styles.iconHint}>
											<Trans>JPEG, PNG, WebP. Max 10MB. Recommended: 512×512px</Trans>
										</div>
									</div>
								</div>
								{form.formState.errors.icon?.message && (
									<p className={styles.iconError}>{form.formState.errors.icon.message}</p>
								)}
							</div>

							<Input
								{...form.register('name')}
								type="text"
								label={t`Group Name`}
								placeholder={t`My Group`}
								minLength={1}
								maxLength={100}
								error={form.formState.errors.name?.message}
							/>

							<div className={styles.footer}>
								<Button type="submit" submitting={isSubmitting} className={styles.fullWidth}>
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
