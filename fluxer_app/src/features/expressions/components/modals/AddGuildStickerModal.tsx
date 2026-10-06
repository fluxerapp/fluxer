// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import {GlobalLimits} from '@app/features/app/utils/GlobalLimits';
import * as GuildStickerCommands from '@app/features/expressions/commands/GuildStickerCommands';
import styles from '@app/features/expressions/components/modals/AddGuildStickerModal.module.css';
import {StickerFormFields} from '@app/features/expressions/components/modals/sticker_form/StickerFormFields';
import {StickerPreview} from '@app/features/expressions/components/modals/sticker_form/StickerPreview';
import * as ImageCropUtils from '@app/features/expressions/utils/ImageCropUtils';
import {CREATE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {failureCode, failureValidationErrors} from '@app/features/platform/utils/ResponseInspection';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Form} from '@app/features/ui/components/form/Form';
import * as FormUtils from '@app/lib/forms';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useState} from 'react';
import {useForm} from 'react-hook-form';

const COULD_NOT_CREATE_STICKER_DESCRIPTOR = msg({
	message: "Couldn't create this sticker",
	comment: 'Title of the generic fallback error modal shown when creating a sticker fails.',
});
const COULD_NOT_PREPARE_STICKER_DESCRIPTOR = msg({
	message: 'Something went wrong while preparing this sticker. Try another file or try again in a moment.',
	comment: 'Body of the generic fallback error modal shown when preparing a sticker upload fails.',
});
const STICKER_FILE_TOO_LARGE_DESCRIPTOR = msg({
	message: 'Sticker is too large',
	comment: 'Title of the error modal shown when a sticker upload exceeds the file size limit.',
});
const STICKER_COULD_NOT_BE_COMPRESSED_DESCRIPTOR = msg({
	message: "Sticker couldn't be compressed enough",
	comment: 'Title of the error modal shown when a sticker remains too large after automatic resizing and compression.',
});
const STICKER_SOURCE_FILE_TOO_LARGE_DESCRIPTOR = msg({
	message:
		'This file is {fileSize}, over the {maxSize} limit. Animated stickers and SVGs must already be small enough to upload. Choose a smaller file or shorten the animation.',
	comment:
		'Body of the sticker upload size error modal for animated files or SVGs. {fileSize} and {maxSize} are formatted file sizes.',
});
const STICKER_PROCESSED_FILE_TOO_LARGE_DESCRIPTOR = msg({
	message:
		'We resized and compressed this sticker to 320x320 pixels, but it is still {fileSize}. The limit is {maxSize}. Try a simpler image, fewer colors, or a smaller source file.',
	comment:
		'Body of the sticker upload size error modal when a static image remains too large after resizing. {fileSize} and {maxSize} are formatted file sizes.',
});
const STICKER_UPLOAD_LIMIT_DESCRIPTOR = msg({
	message:
		'The upload service rejected this sticker as larger than {maxSize}. Try a smaller file, a simpler image, or a shorter animation.',
	comment: 'Body of the sticker upload size error modal when the upload service rejects the image as too large.',
});
const STICKER_SIZE_ERROR_FILE_DESCRIPTOR = msg({
	message: 'File: {fileName}',
	comment: 'File name row in the sticker upload size error modal. {fileName} is the uploaded file name.',
});
const BACK_TO_STICKER_DESCRIPTOR = msg({
	message: 'Back to sticker',
	comment: 'Primary button label in the sticker upload error modal that returns to the sticker form.',
});
const ADD_STICKER_DESCRIPTOR = msg({
	message: 'Add sticker',
	comment: 'Action that opens the add-sticker modal.',
});
const ADD_STICKER_FORM_DESCRIPTOR = msg({
	message: 'Add sticker form',
	comment: 'Accessible label for the add-sticker form.',
});
const logger = new Logger('AddGuildStickerModal');

const STICKER_SIZE_VALIDATION_CODES = new Set<string>([
	ValidationErrorCodes.BASE64_LENGTH_INVALID,
	ValidationErrorCodes.IMAGE_SIZE_EXCEEDS_LIMIT,
]);

interface AddGuildStickerModalProps {
	guildId: string;
	file: File;
	onSuccess: () => void;
}

interface FormInputs {
	name: string;
	description: string;
	tags: Array<string>;
}

function isStickerSizeValidationError(error: unknown): boolean {
	if (failureCode(error) === APIErrorCodes.FILE_SIZE_TOO_LARGE) {
		return true;
	}
	return (
		failureValidationErrors(error)?.some((validationError) => {
			const code = 'code' in validationError && typeof validationError.code === 'string' ? validationError.code : null;
			return validationError.path === 'image' && STICKER_SIZE_VALIDATION_CODES.has(code ?? validationError.message);
		}) ?? false
	);
}

function renderStickerSizeErrorDescription(message: string, fileName: string, i18n: I18n) {
	return (
		<div className={styles.errorModalContent} data-flx="expressions.add-guild-sticker-modal.size-error-content">
			<p className={styles.errorModalMessage} data-flx="expressions.add-guild-sticker-modal.size-error-message">
				{message}
			</p>
			<div className={styles.errorModalFileName} data-flx="expressions.add-guild-sticker-modal.size-error-file-name">
				{i18n._(STICKER_SIZE_ERROR_FILE_DESCRIPTOR, {fileName})}
			</div>
		</div>
	);
}

export const AddGuildStickerModal = observer(function AddGuildStickerModal({
	guildId,
	file,
	onSuccess,
}: AddGuildStickerModalProps) {
	const {i18n} = useLingui();
	const [isProcessing, setIsProcessing] = useState(false);
	const [previewUrl, setPreviewUrl] = useState<string | null>(null);
	const form = useForm<FormInputs>({
		defaultValues: {
			name: GuildStickerCommands.sanitizeStickerName(file.name),
			description: '',
			tags: [],
		},
	});
	useEffect(() => {
		const url = URL.createObjectURL(file);
		setPreviewUrl(url);
		return () => URL.revokeObjectURL(url);
	}, [file]);
	const onSubmit = useCallback(
		async (data: FormInputs) => {
			const maxStickerSize = GlobalLimits.getStickerMaxSize();
			setIsProcessing(true);
			try {
				const base64Image = await ImageCropUtils.optimizeStickerImage(file, maxStickerSize, 320);
				await GuildStickerCommands.create(guildId, {
					name: data.name.trim(),
					description: data.description.trim(),
					tags: data.tags.length > 0 ? data.tags : [],
					image: base64Image,
				});
				onSuccess();
				ModalCommands.pop();
			} catch (error: unknown) {
				logger.error('Failed to create sticker:', error);
				if (error instanceof ImageCropUtils.ImageOptimizationSizeError || isStickerSizeValidationError(error)) {
					const fileSize =
						error instanceof ImageCropUtils.ImageOptimizationSizeError
							? formatFileSize(i18n.locale, error.actualSizeBytes)
							: formatFileSize(i18n.locale, file.size);
					const maxSize = formatFileSize(i18n.locale, maxStickerSize);
					const message =
						error instanceof ImageCropUtils.ImageOptimizationSizeError
							? i18n._(
									error.reason === 'processed'
										? STICKER_PROCESSED_FILE_TOO_LARGE_DESCRIPTOR
										: STICKER_SOURCE_FILE_TOO_LARGE_DESCRIPTOR,
									{fileSize, maxSize},
								)
							: i18n._(STICKER_UPLOAD_LIMIT_DESCRIPTOR, {maxSize});
					const title =
						error instanceof ImageCropUtils.ImageOptimizationSizeError && error.reason === 'processed'
							? i18n._(STICKER_COULD_NOT_BE_COMPRESSED_DESCRIPTOR)
							: i18n._(STICKER_FILE_TOO_LARGE_DESCRIPTOR);
					ModalCommands.push(
						ModalCommands.modal(() => (
							<ConfirmModal
								title={title}
								description={renderStickerSizeErrorDescription(message, file.name, i18n)}
								primaryText={i18n._(BACK_TO_STICKER_DESCRIPTOR)}
								onPrimary={() => {}}
								secondaryText={false}
								data-flx="expressions.add-guild-sticker-modal.create.size-error-modal"
							/>
						)),
					);
				} else if (error instanceof HttpError) {
					FormUtils.handleError(i18n, form, error, 'name');
				} else {
					ModalCommands.push(
						ModalCommands.modal(() => (
							<GenericErrorModal
								title={i18n._(COULD_NOT_CREATE_STICKER_DESCRIPTOR)}
								message={i18n._(COULD_NOT_PREPARE_STICKER_DESCRIPTOR)}
								data-flx="expressions.add-guild-sticker-modal.create.generic-error-modal"
							/>
						)),
					);
				}
				setIsProcessing(false);
			}
		},
		[guildId, file, onSuccess, form, i18n],
	);
	const {handleSubmit: handleSave} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'name',
	});
	return (
		<Modal.Root size="small" centered data-flx="expressions.add-guild-sticker-modal.modal-root">
			<Modal.Header
				title={i18n._(ADD_STICKER_DESCRIPTOR)}
				data-flx="expressions.add-guild-sticker-modal.modal-header"
			/>
			<Modal.Content data-flx="expressions.add-guild-sticker-modal.modal-content">
				<Form
					form={form}
					onSubmit={handleSave}
					aria-label={i18n._(ADD_STICKER_FORM_DESCRIPTOR)}
					data-flx="expressions.add-guild-sticker-modal.form.save"
				>
					<div className={styles.formContainer} data-flx="expressions.add-guild-sticker-modal.form-container">
						{previewUrl && (
							<StickerPreview
								imageUrl={previewUrl}
								altText={form.watch('name') || file.name}
								data-flx="expressions.add-guild-sticker-modal.sticker-preview"
							/>
						)}
						<StickerFormFields
							form={form}
							disabled={isProcessing}
							data-flx="expressions.add-guild-sticker-modal.sticker-form-fields"
						/>
					</div>
				</Form>
			</Modal.Content>
			<Modal.Footer data-flx="expressions.add-guild-sticker-modal.modal-footer">
				<Button
					variant="secondary"
					onClick={() => ModalCommands.pop()}
					disabled={isProcessing}
					data-flx="expressions.add-guild-sticker-modal.button.pop"
				>
					<Trans>Cancel</Trans>
				</Button>
				<Button
					onClick={handleSave}
					disabled={!form.watch('name')?.trim() || isProcessing}
					submitting={isProcessing}
					data-flx="expressions.add-guild-sticker-modal.button.save"
				>
					{i18n._(CREATE_DESCRIPTOR)}
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
