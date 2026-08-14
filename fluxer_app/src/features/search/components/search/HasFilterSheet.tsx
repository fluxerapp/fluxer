// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/search/components/search/HasFilterSheet.module.css';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import {Button} from '@app/features/ui/button/Button';
import {Scroller} from '@app/features/ui/components/Scroller';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import type {IconProps} from '@phosphor-icons/react';
import {
	BrowserIcon,
	CheckIcon,
	FileIcon,
	ImageIcon,
	LinkIcon,
	MusicNoteIcon,
	StickerIcon,
	VideoIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';

const IMAGE_UPLOAD_DESCRIPTOR = msg({
	message: 'Image upload',
	comment: 'Label for the has:image option in the mobile content-type filter sheet.',
});
const UPLOADED_IMAGE_FILES_ONLY_DESCRIPTOR = msg({
	message: 'Uploaded image files only',
	comment: 'Description text under the has:image option in the mobile content-type filter sheet.',
});
const VIDEO_UPLOAD_DESCRIPTOR = msg({
	message: 'Video upload',
	comment: 'Label for the has:video option in the mobile content-type filter sheet.',
});
const UPLOADED_VIDEO_FILES_ONLY_DESCRIPTOR = msg({
	message: 'Uploaded video files only',
	comment: 'Description text under the has:video option in the mobile content-type filter sheet.',
});
const AUDIO_UPLOAD_DESCRIPTOR = msg({
	message: 'Audio upload',
	comment: 'Label for the has:sound option in the mobile content-type filter sheet.',
});
const UPLOADED_AUDIO_FILES_ONLY_DESCRIPTOR = msg({
	message: 'Uploaded audio files only',
	comment: 'Description text under the has:sound option in the mobile content-type filter sheet.',
});
const FILE_UPLOAD_DESCRIPTOR = msg({
	message: 'File upload',
	comment: 'Label for the has:file option in the mobile content-type filter sheet.',
});
const ANY_UPLOADED_ATTACHMENT_DESCRIPTOR = msg({
	message: 'Any uploaded attachment',
	comment: 'Description text under the has:file option in the mobile content-type filter sheet.',
});
const LINK_DESCRIPTOR = msg({
	message: 'Link',
	comment: 'Label for the has:link option in the mobile content-type filter sheet.',
});
const TYPED_URL_IN_THE_MESSAGE_TEXT_DESCRIPTOR = msg({
	message: 'Typed URL in the message text',
	comment: 'Description text under the has:link option in the mobile content-type filter sheet.',
});
const LINK_PREVIEW_OR_EMBED_DESCRIPTOR = msg({
	message: 'Link preview or embed',
	comment: 'Label for the has:embed option in the mobile content-type filter sheet.',
});
const RESOLVED_PREVIEWS_AND_RICH_EMBEDS_NOT_UPLOADS_DESCRIPTOR = msg({
	message: 'Resolved previews and rich embeds, not uploads',
	comment: 'Description text under the has:embed option in the mobile content-type filter sheet.',
});
const STICKER_DESCRIPTOR = msg({
	message: 'Sticker',
	comment: 'Label for the has:sticker option in the mobile content-type filter sheet.',
});
const STICKER_ATTACHED_TO_THE_MESSAGE_DESCRIPTOR = msg({
	message: 'Sticker attached to the message',
	comment: 'Description text under the has:sticker option in the mobile content-type filter sheet.',
});
const FILTER_BY_CONTENT_DESCRIPTOR = msg({
	message: 'Filter by content',
	comment: 'Title of the mobile content-type (has:) filter sheet in message search.',
});

export type HasFilterType = 'image' | 'sound' | 'video' | 'file' | 'sticker' | 'embed' | 'link';

interface HasFilterOption {
	type: HasFilterType;
	label: MessageDescriptor;
	description: MessageDescriptor;
	icon: React.ComponentType<IconProps>;
}

const HAS_FILTER_OPTIONS: Array<HasFilterOption> = [
	{type: 'image', label: IMAGE_UPLOAD_DESCRIPTOR, description: UPLOADED_IMAGE_FILES_ONLY_DESCRIPTOR, icon: ImageIcon},
	{type: 'video', label: VIDEO_UPLOAD_DESCRIPTOR, description: UPLOADED_VIDEO_FILES_ONLY_DESCRIPTOR, icon: VideoIcon},
	{
		type: 'sound',
		label: AUDIO_UPLOAD_DESCRIPTOR,
		description: UPLOADED_AUDIO_FILES_ONLY_DESCRIPTOR,
		icon: MusicNoteIcon,
	},
	{type: 'file', label: FILE_UPLOAD_DESCRIPTOR, description: ANY_UPLOADED_ATTACHMENT_DESCRIPTOR, icon: FileIcon},
	{type: 'link', label: LINK_DESCRIPTOR, description: TYPED_URL_IN_THE_MESSAGE_TEXT_DESCRIPTOR, icon: LinkIcon},
	{
		type: 'embed',
		label: LINK_PREVIEW_OR_EMBED_DESCRIPTOR,
		description: RESOLVED_PREVIEWS_AND_RICH_EMBEDS_NOT_UPLOADS_DESCRIPTOR,
		icon: BrowserIcon,
	},
	{
		type: 'sticker',
		label: STICKER_DESCRIPTOR,
		description: STICKER_ATTACHED_TO_THE_MESSAGE_DESCRIPTOR,
		icon: StickerIcon,
	},
];

interface HasFilterSheetProps {
	isOpen: boolean;
	onClose: () => void;
	selectedFilters: Array<HasFilterType>;
	onFiltersChange: (filters: Array<HasFilterType>) => void;
}

export const HasFilterSheet: React.FC<HasFilterSheetProps> = ({isOpen, onClose, selectedFilters, onFiltersChange}) => {
	const {i18n} = useLingui();
	const toggleFilter = (type: HasFilterType) => {
		if (selectedFilters.includes(type)) {
			onFiltersChange(selectedFilters.filter((f) => f !== type));
		} else {
			onFiltersChange([...selectedFilters, type]);
		}
	};
	return (
		<BottomSheet
			isOpen={isOpen}
			onClose={onClose}
			snapPoints={[0, 1]}
			initialSnap={1}
			title={i18n._(FILTER_BY_CONTENT_DESCRIPTOR)}
			disablePadding
			data-flx="search.search.has-filter-sheet.bottom-sheet"
		>
			<div className={styles.container} data-flx="search.search.has-filter-sheet.container">
				<p className={styles.subtitle} data-flx="search.search.has-filter-sheet.subtitle">
					<Trans>Show messages that contain:</Trans>
				</p>
				<Scroller
					key="has-filter-scroller"
					className={styles.scroller}
					fade={false}
					data-flx="search.search.has-filter-sheet.scroller"
				>
					<div className={styles.optionsContainer} data-flx="search.search.has-filter-sheet.options-container">
						{HAS_FILTER_OPTIONS.map((option) => {
							const isSelected = selectedFilters.includes(option.type);
							const Icon = option.icon;
							return (
								<button
									key={option.type}
									type="button"
									aria-pressed={isSelected}
									className={clsx(styles.option, isSelected && styles.optionSelected)}
									onClick={() => toggleFilter(option.type)}
									data-flx="search.search.has-filter-sheet.option.toggle-filter.button"
								>
									<div className={styles.optionLeft} data-flx="search.search.has-filter-sheet.option-left">
										<Icon
											size={remFromPx(22)}
											className={clsx(styles.optionIcon, isSelected && styles.optionIconSelected)}
											weight="regular"
											data-flx="search.search.has-filter-sheet.option-icon"
										/>
										<span className={styles.optionText} data-flx="search.search.has-filter-sheet.option-text">
											<span
												className={clsx(styles.optionLabel, isSelected && styles.optionLabelSelected)}
												data-flx="search.search.has-filter-sheet.option-label"
											>
												{i18n._(option.label)}
											</span>
											<span
												className={styles.optionDescription}
												data-flx="search.search.has-filter-sheet.option-description"
											>
												{i18n._(option.description)}
											</span>
										</span>
									</div>
									{isSelected && (
										<CheckIcon
											size={remFromPx(20)}
											className={styles.checkIcon}
											weight="bold"
											data-flx="search.search.has-filter-sheet.check-icon"
										/>
									)}
								</button>
							);
						})}
					</div>
				</Scroller>
				<div className={styles.footer} data-flx="search.search.has-filter-sheet.footer">
					<Button variant="primary" onClick={onClose} data-flx="search.search.has-filter-sheet.button.close">
						<Trans>Done</Trans>
					</Button>
				</div>
			</div>
		</BottomSheet>
	);
};
