// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {Limits} from '@app/features/app/utils/UserLimits';
import {CANCEL_DESCRIPTOR, GET_PREMIUM_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import * as PremiumModalCommands from '@app/features/premium/commands/PremiumModalCommands';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import Users from '@app/features/user/state/Users';
import {ATTACHMENT_MAX_SIZE_PREMIUM} from '@fluxer/constants/src/LimitConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

const SOME_FILES_YOU_RE_TRYING_TO_UPLOAD_EXCEED_DESCRIPTOR = msg({
	message: "Some files you're trying to upload exceed the maximum size limit of {maxSizeFormatted} per file.",
	comment:
		'Body of the file-too-large alert when multiple files exceed the per-file size limit. maxSizeFormatted is the limit formatted with units.',
});
const THE_FILE_YOU_RE_TRYING_TO_UPLOAD_EXCEEDS_DESCRIPTOR = msg({
	message: "The file you're trying to upload exceeds the maximum size limit of {maxSizeFormatted}.",
	comment: 'Body of the file-too-large alert when a single file exceeds the per-file size limit.',
});
const ONE_OR_MORE_FILES_YOU_RE_TRYING_TO_DESCRIPTOR = msg({
	message: "One or more files you're trying to upload exceed the maximum size limit of {maxSizeFormatted} per file.",
	comment: 'Body of the file-too-large alert for ambiguous count (single or multiple) over the per-file size limit.',
});
const FILE_SIZE_TOO_LARGE_DESCRIPTOR = msg({
	message: 'File size too large',
	comment: 'Title of the file-too-large alert.',
});
const FILE_SIZE_LIMIT_EXCEEDED_DESCRIPTOR = msg({
	message: 'File size limit exceeded',
	comment: 'Title of the file-too-large alert variant that offers a Plutonium upsell.',
});
const INSTANCE_ADMIN_LIMIT_DESCRIPTOR = msg({
	message: 'This limit is configured by your instance administrator.',
	comment: 'Extra file-too-large alert sentence shown when premium upload-limit upgrades are unavailable.',
});
const PREMIUM_FILE_SIZE_UPSELL_DESCRIPTOR = msg({
	message:
		'With {premiumProductName}, your per-file upload limit increases to {fileSizeLimit}, plus animated avatars, longer messages, and many other {premiumProductName} perks.',
	comment:
		'Premium upsell sentence in the file-too-large alert. premiumProductName is the paid product name; fileSizeLimit is a localized file size.',
});

interface FileSizeTooLargeModalProps {
	oversizedFileCount?: number;
}

export const FileSizeTooLargeModal = observer(({oversizedFileCount}: FileSizeTooLargeModalProps) => {
	const {i18n} = useLingui();
	const user = Users.currentUser;
	const showPremium = shouldShowPremiumFeatures();
	const maxAttachmentFileSize = user?.maxAttachmentFileSize ?? 25 * 1024 * 1024;
	const premiumMaxAttachmentFileSize = Limits.getPremiumValue('max_attachment_file_size', ATTACHMENT_MAX_SIZE_PREMIUM);
	const canUpgradeAttachmentLimit = maxAttachmentFileSize < premiumMaxAttachmentFileSize;
	const maxSizeFormatted = formatFileSize(maxAttachmentFileSize);
	const hasKnownOversizedFileCount = oversizedFileCount != null;
	const hasMultipleOversizedFiles = (oversizedFileCount ?? 0) > 1;
	const handleGetPlutoniumClick = useCallback(() => {
		window.setTimeout(() => {
			PremiumModalCommands.open();
		}, 0);
	}, []);
	const baseDescription = hasKnownOversizedFileCount
		? hasMultipleOversizedFiles
			? i18n._(SOME_FILES_YOU_RE_TRYING_TO_UPLOAD_EXCEED_DESCRIPTOR, {maxSizeFormatted})
			: i18n._(THE_FILE_YOU_RE_TRYING_TO_UPLOAD_EXCEEDS_DESCRIPTOR, {maxSizeFormatted})
		: i18n._(ONE_OR_MORE_FILES_YOU_RE_TRYING_TO_DESCRIPTOR, {maxSizeFormatted});
	if (!showPremium || !canUpgradeAttachmentLimit) {
		return (
			<GenericErrorModal
				title={i18n._(FILE_SIZE_TOO_LARGE_DESCRIPTOR)}
				message={!showPremium ? `${baseDescription} ${i18n._(INSTANCE_ADMIN_LIMIT_DESCRIPTOR)}` : baseDescription}
				data-flx="messaging.file-size-too-large-modal.confirm-modal"
			/>
		);
	}
	return (
		<ConfirmModal
			title={i18n._(FILE_SIZE_LIMIT_EXCEEDED_DESCRIPTOR)}
			description={`${baseDescription} ${i18n._(PREMIUM_FILE_SIZE_UPSELL_DESCRIPTOR, {
				premiumProductName: PREMIUM_PRODUCT_NAME,
				fileSizeLimit: formatFileSize(premiumMaxAttachmentFileSize),
			})}`}
			primaryText={i18n._(GET_PREMIUM_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
			primaryVariant="primary"
			onPrimary={handleGetPlutoniumClick}
			secondaryText={i18n._(CANCEL_DESCRIPTOR)}
			data-flx="messaging.file-size-too-large-modal.confirm-modal--2"
		/>
	);
});
