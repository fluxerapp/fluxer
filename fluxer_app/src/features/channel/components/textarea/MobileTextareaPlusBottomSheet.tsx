// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as PremiumModalCommands from '@app/features/premium/commands/PremiumModalCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {MenuBottomSheet, type MenuGroupType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {useLingui} from '@lingui/react/macro';
import {GiftIcon, PaperclipIcon, TableIcon, UploadSimpleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useMemo} from 'react';
import {
	CREATE_POLL_DESCRIPTOR,
	SEND_GIFT_DESCRIPTOR,
	UPLOAD_FILE_DESCRIPTOR,
	UPLOAD_YOUR_MESSAGE_AS_A_FILE_DESCRIPTOR,
} from './TextareaPlusMenu';

interface MobileTextareaPlusBottomSheetProps {
	canSendPolls: boolean;
	isOpen: boolean;
	onClose: () => void;
	onUploadFile: () => void;
	onSendPoll: () => void;
	textareaValue?: string;
	onUploadAsFile?: () => void;
}

export const MobileTextareaPlusBottomSheet = observer(
	({canSendPolls, isOpen, onClose, onUploadFile, onSendPoll, textareaValue, onUploadAsFile}: MobileTextareaPlusBottomSheetProps) => {
		const {i18n} = useLingui();
		const isSelfHosted = RuntimeConfig.isSelfHosted();
		const groups: Array<MenuGroupType> = useMemo(() => {
			const items = [
				{
					icon: (
						<PaperclipIcon
							weight="bold"
							data-flx="channel.textarea.mobile-textarea-plus-bottom-sheet.groups.paperclip-icon"
						/>
					),
					label: i18n._(UPLOAD_FILE_DESCRIPTOR),
					onClick: () => {
						onUploadFile();
						onClose();
					},
				},
			];
			const hasTextContent = textareaValue && textareaValue.trim().length > 0;
			if (hasTextContent && onUploadAsFile) {
				items.push({
					icon: (
						<UploadSimpleIcon data-flx="channel.textarea.mobile-textarea-plus-bottom-sheet.groups.upload-simple-icon" />
					),
					label: i18n._(UPLOAD_YOUR_MESSAGE_AS_A_FILE_DESCRIPTOR),
					onClick: () => {
						onUploadAsFile();
						onClose();
					},
				});
			}
			if (canSendPolls) {
				items.push({
					icon: (
						<TableIcon weight="bold" data-flx="channel.textarea.mobile-textarea-plus-bottom-sheet.groups.table-icon" />
					),
					label: i18n._(CREATE_POLL_DESCRIPTOR),
					onClick: () => {
						onSendPoll();
						onClose();
					},
				});
			}
			if (!isSelfHosted) {
				items.push({
					icon: <GiftIcon data-flx="channel.textarea.mobile-textarea-plus-bottom-sheet.groups.gift-icon" />,
					label: i18n._(SEND_GIFT_DESCRIPTOR),
					onClick: () => {
						ModalCommands.runAfterBottomSheetClose(onClose, () => PremiumModalCommands.open(true));
					},
				});
			}
			return [{items}];
		}, [isSelfHosted, onClose, onUploadFile, textareaValue, onUploadAsFile, i18n.locale]);
		return (
			<MenuBottomSheet
				isOpen={isOpen}
				onClose={onClose}
				groups={groups}
				data-flx="channel.textarea.mobile-textarea-plus-bottom-sheet.menu-bottom-sheet"
			/>
		);
	},
);

MobileTextareaPlusBottomSheet.displayName = 'MobileTextareaPlusBottomSheet';
