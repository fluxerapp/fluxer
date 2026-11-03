// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {Limits} from '@app/features/app/utils/UserLimits';
import {CANCEL_DESCRIPTOR, GET_PREMIUM_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {SafeMarkdown} from '@app/features/messaging/components/markdown';
import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import * as PremiumModalCommands from '@app/features/premium/commands/PremiumModalCommands';
import {getPremiumProductName, shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import Users from '@app/features/user/state/Users';
import {ATTACHMENT_MAX_SIZE_NON_PREMIUM, ATTACHMENT_MAX_SIZE_PREMIUM} from '@fluxer/constants/src/LimitConstants';
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
		'With {premiumProductName}, your per-file upload limit increases to {fileSizeLimit}. You also get animated avatars, longer messages, and many other {premiumProductName} perks.',
	comment:
		'Premium upsell sentence in the file-too-large alert. premiumProductName is the paid product name; fileSizeLimit is a localized file size.',
});

function withInstanceNote(description: string, note: string | null): React.ReactNode {
	if (!note) return description;
	return (
		<>
			<span data-flx="messaging.file-size-too-large-modal.description">{description}</span>
			<div className={markupStyles.markup} data-flx="messaging.file-size-too-large-modal.instance-note">
				<SafeMarkdown
					content={note}
					options={{context: MarkdownContext.RESTRICTED_EMBED_DESCRIPTION, disableEmojiInfoCard: true}}
					data-flx="messaging.file-size-too-large-modal.instance-note-markdown"
				/>
			</div>
		</>
	);
}

interface FileSizeTooLargeModalProps {
	oversizedFileCount?: number;
}

export const FileSizeTooLargeModal = observer(({oversizedFileCount}: FileSizeTooLargeModalProps) => {
	const {i18n} = useLingui();
	const user = Users.currentUser;
	const showPremium = shouldShowPremiumFeatures();
	const maxAttachmentFileSize = user?.maxAttachmentFileSize ?? ATTACHMENT_MAX_SIZE_NON_PREMIUM;
	const premiumMaxAttachmentFileSize = Limits.getStockValue('max_attachment_file_size', ATTACHMENT_MAX_SIZE_PREMIUM);
	const canUpgradeAttachmentLimit = maxAttachmentFileSize < premiumMaxAttachmentFileSize;
	const maxSizeFormatted = formatFileSize(i18n.locale, maxAttachmentFileSize);
	const hasKnownOversizedFileCount = oversizedFileCount != null;
	const hasMultipleOversizedFiles = (oversizedFileCount ?? 0) > 1;
	const instanceNote = RuntimeConfig.appPublic.branding.upload_limit_message;
	const handleGetPlutoniumClick = useCallback(() => {
		window.setTimeout(() => {
			PremiumModalCommands.open();
		}, 0);
	}, []);
	let baseDescription: string;
	if (!hasKnownOversizedFileCount) {
		baseDescription = i18n._(ONE_OR_MORE_FILES_YOU_RE_TRYING_TO_DESCRIPTOR, {maxSizeFormatted});
	} else if (hasMultipleOversizedFiles) {
		baseDescription = i18n._(SOME_FILES_YOU_RE_TRYING_TO_UPLOAD_EXCEED_DESCRIPTOR, {maxSizeFormatted});
	} else {
		baseDescription = i18n._(THE_FILE_YOU_RE_TRYING_TO_UPLOAD_EXCEEDS_DESCRIPTOR, {maxSizeFormatted});
	}
	if (!showPremium || !canUpgradeAttachmentLimit) {
		const title = i18n._(FILE_SIZE_TOO_LARGE_DESCRIPTOR);
		let message: React.ReactNode = baseDescription;
		if (instanceNote) {
			message = withInstanceNote(baseDescription, instanceNote);
		} else if (!showPremium) {
			message = `${baseDescription} ${i18n._(INSTANCE_ADMIN_LIMIT_DESCRIPTOR)}`;
		}
		return (
			<GenericErrorModal title={title} message={message} data-flx="messaging.file-size-too-large-modal.confirm-modal" />
		);
	}
	return (
		<ConfirmModal
			title={i18n._(FILE_SIZE_LIMIT_EXCEEDED_DESCRIPTOR)}
			description={withInstanceNote(
				`${baseDescription} ${i18n._(PREMIUM_FILE_SIZE_UPSELL_DESCRIPTOR, {
					premiumProductName: getPremiumProductName(),
					fileSizeLimit: formatFileSize(i18n.locale, premiumMaxAttachmentFileSize),
				})}`,
				instanceNote,
			)}
			primaryText={i18n._(GET_PREMIUM_DESCRIPTOR, {premiumProductName: getPremiumProductName()})}
			primaryVariant="primary"
			onPrimary={handleGetPlutoniumClick}
			secondaryText={i18n._(CANCEL_DESCRIPTOR)}
			data-flx="messaging.file-size-too-large-modal.confirm-modal--2"
		/>
	);
});
