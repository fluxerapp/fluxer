// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {EXAMPLE_URL} from '@app/features/app/config/I18nDisplayConstants';
import {CANCEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Translation from '@app/features/messaging/state/Translation';
import ReverseImageSearch from '@app/features/search/state/ReverseImageSearch';
import SearchEngine from '@app/features/search/state/SearchEngine';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Input} from '@app/features/ui/components/form/FormInput';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useState} from 'react';

const ADD_REVERSE_IMAGE_SEARCH_ENGINE_DESCRIPTOR = msg({
	message: 'Add reverse image search engine',
	comment: 'Button or menu action label in the add custom search engine modal. Keep it concise.',
});
const ADD_TRANSLATION_PROVIDER_DESCRIPTOR = msg({
	message: 'Add translation provider',
	comment: 'Button or menu action label in the add custom search engine modal. Keep it concise.',
});
const ADD_SEARCH_ENGINE_DESCRIPTOR = msg({
	message: 'Add search engine',
	comment: 'Button or menu action label in the add custom search engine modal. Keep it concise.',
});
const EDIT_REVERSE_IMAGE_SEARCH_ENGINE_DESCRIPTOR = msg({
	message: 'Edit reverse image search engine',
	comment: 'Button or menu action label in the add custom search engine modal. Keep it concise.',
});
const EDIT_TRANSLATION_PROVIDER_DESCRIPTOR = msg({
	message: 'Edit translation provider',
	comment: 'Button or menu action label in the add custom search engine modal. Keep it concise.',
});
const EDIT_SEARCH_ENGINE_DESCRIPTOR = msg({
	message: 'Edit search engine',
	comment: 'Button or menu action label in the add custom search engine modal. Keep it concise.',
});
const USE_WHERE_THE_IMAGE_URL_SHOULD_BE_INSERTED_DESCRIPTOR = msg({
	message: 'Use {url} where the image URL should be inserted.',
	comment: 'Description text in the add custom search engine modal. Preserve {url}; it is inserted by code.',
});
const USE_WHERE_THE_TEXT_TO_TRANSLATE_SHOULD_BE_DESCRIPTOR = msg({
	message: 'Use {query} where the text to translate should be inserted.',
	comment: 'Description text in the add custom search engine modal. Preserve {query}; it is inserted by code.',
});
const USE_WHERE_THE_SEARCH_TEXT_SHOULD_BE_INSERTED_DESCRIPTOR = msg({
	message: 'Use {query} where the search text should be inserted.',
	comment: 'Description text in the add custom search engine modal. Preserve {query}; it is inserted by code.',
});
const URL_PATTERN_MUST_CONTAIN_PLACEHOLDER_DESCRIPTOR = msg({
	message: 'URL pattern must contain the {url} placeholder.',
	comment:
		'Placeholder text in the add custom search engine modal. Keep it concise. Preserve {url}; it is inserted by code.',
});
const URL_PATTERN_MUST_CONTAIN_PLACEHOLDER_2_DESCRIPTOR = msg({
	message: 'URL pattern must contain the {query} placeholder.',
	comment:
		'Placeholder text in the add custom search engine modal. Keep it concise. Preserve {query}; it is inserted by code.',
});
const NAME_IS_REQUIRED_DESCRIPTOR = msg({
	message: 'Name is required.',
	comment: 'Short label in the add custom search engine modal. Keep it concise.',
});
const URL_PATTERN_IS_REQUIRED_DESCRIPTOR = msg({
	message: 'URL pattern is required.',
	comment: 'Label in the add custom search engine modal.',
});
const URL_PATTERN_MUST_BE_A_VALID_URL_DESCRIPTOR = msg({
	message: 'URL pattern must be a valid URL.',
	comment: 'Description text in the add custom search engine modal.',
});
const NAME_DESCRIPTOR = msg({
	message: 'Name',
	comment: 'Short label in the add custom search engine modal. Keep it concise.',
});
const MY_REVERSE_IMAGE_SEARCH_DESCRIPTOR = msg({
	message: 'My reverse image search',
	comment: 'Label in the add custom search engine modal.',
});
const MY_TRANSLATOR_DESCRIPTOR = msg({
	message: 'My translator',
	comment: 'Short label in the add custom search engine modal. Keep it concise.',
});
const MY_SEARCH_ENGINE_DESCRIPTOR = msg({
	message: 'My search engine',
	comment: 'Short label in the add custom search engine modal. Keep it concise.',
});
const URL_PATTERN_DESCRIPTOR = msg({
	message: 'URL pattern',
	comment: 'Short label in the add custom search engine modal. Keep it concise.',
});
const SAVE_DESCRIPTOR = msg({
	message: 'Save',
	comment: 'Button or menu action label in the add custom search engine modal. Keep it concise.',
});
const ADD_DESCRIPTOR = msg({
	message: 'Add',
	comment: 'Button or menu action label in the add custom search engine modal. Keep it concise.',
});

export type SearchEngineMode = 'text' | 'image' | 'translate';

interface AddCustomSearchEngineModalProps {
	mode?: SearchEngineMode;
	editingEngineId?: string;
	initialName?: string;
	initialUrlTemplate?: string;
}

export const AddCustomSearchEngineModal: React.FC<AddCustomSearchEngineModalProps> = observer(
	({mode = 'text', editingEngineId, initialName = '', initialUrlTemplate = ''}) => {
		const {i18n} = useLingui();
		const [name, setName] = useState(initialName);
		const [urlTemplate, setUrlTemplate] = useState(initialUrlTemplate);
		const [error, setError] = useState('');
		const isEditing = editingEngineId != null;
		const isImageMode = mode === 'image';
		const isTranslateMode = mode === 'translate';
		const placeholder = isImageMode ? '{url}' : '{query}';
		const store = isImageMode ? ReverseImageSearch : isTranslateMode ? Translation : SearchEngine;
		const titleAdd = isImageMode
			? i18n._(ADD_REVERSE_IMAGE_SEARCH_ENGINE_DESCRIPTOR)
			: isTranslateMode
				? i18n._(ADD_TRANSLATION_PROVIDER_DESCRIPTOR)
				: i18n._(ADD_SEARCH_ENGINE_DESCRIPTOR);
		const titleEdit = isImageMode
			? i18n._(EDIT_REVERSE_IMAGE_SEARCH_ENGINE_DESCRIPTOR)
			: isTranslateMode
				? i18n._(EDIT_TRANSLATION_PROVIDER_DESCRIPTOR)
				: i18n._(EDIT_SEARCH_ENGINE_DESCRIPTOR);
		const exampleUrl = isImageMode
			? `${EXAMPLE_URL}/searchbyimage?url=${placeholder}`
			: isTranslateMode
				? `${EXAMPLE_URL}/translate?text=${placeholder}`
				: `${EXAMPLE_URL}/search?q=${placeholder}`;
		const hint = isImageMode
			? i18n._(USE_WHERE_THE_IMAGE_URL_SHOULD_BE_INSERTED_DESCRIPTOR, {url: '{url}'})
			: isTranslateMode
				? i18n._(USE_WHERE_THE_TEXT_TO_TRANSLATE_SHOULD_BE_DESCRIPTOR, {query: '{query}'})
				: i18n._(USE_WHERE_THE_SEARCH_TEXT_SHOULD_BE_INSERTED_DESCRIPTOR, {query: '{query}'});
		const placeholderRequiredError = isImageMode
			? i18n._(URL_PATTERN_MUST_CONTAIN_PLACEHOLDER_DESCRIPTOR, {url: '{url}'})
			: i18n._(URL_PATTERN_MUST_CONTAIN_PLACEHOLDER_2_DESCRIPTOR, {query: '{query}'});
		const handleClose = useCallback(() => {
			ModalCommands.pop();
		}, []);
		const handleSubmit = useCallback(() => {
			const trimmedName = name.trim();
			const trimmedUrl = urlTemplate.trim();
			if (!trimmedName) {
				setError(i18n._(NAME_IS_REQUIRED_DESCRIPTOR));
				return;
			}
			if (!trimmedUrl) {
				setError(i18n._(URL_PATTERN_IS_REQUIRED_DESCRIPTOR));
				return;
			}
			if (!trimmedUrl.includes(placeholder)) {
				setError(placeholderRequiredError);
				return;
			}
			try {
				const testUrl = trimmedUrl.replace(placeholder, 'test');
				new URL(testUrl);
			} catch {
				setError(i18n._(URL_PATTERN_MUST_BE_A_VALID_URL_DESCRIPTOR));
				return;
			}
			if (isEditing) {
				store.updateCustomEngine(editingEngineId, trimmedName, trimmedUrl);
			} else {
				store.addCustomEngine(trimmedName, trimmedUrl);
			}
			ModalCommands.pop();
		}, [editingEngineId, isEditing, name, placeholder, placeholderRequiredError, store, urlTemplate, i18n]);
		return (
			<Modal.Root
				size="small"
				onClose={handleClose}
				data-flx="user.chat-settings-tab.add-custom-search-engine-modal.modal-root"
			>
				<Modal.Header
					title={isEditing ? titleEdit : titleAdd}
					onClose={handleClose}
					data-flx="user.chat-settings-tab.add-custom-search-engine-modal.modal-header"
				/>
				<Modal.Content data-flx="user.chat-settings-tab.add-custom-search-engine-modal.modal-content">
					<Modal.ContentLayout data-flx="user.chat-settings-tab.add-custom-search-engine-modal.modal-content-layout">
						<Input
							label={i18n._(NAME_DESCRIPTOR)}
							value={name}
							onChange={(event) => {
								setName(event.target.value);
								setError('');
							}}
							placeholder={
								isImageMode
									? i18n._(MY_REVERSE_IMAGE_SEARCH_DESCRIPTOR)
									: isTranslateMode
										? i18n._(MY_TRANSLATOR_DESCRIPTOR)
										: i18n._(MY_SEARCH_ENGINE_DESCRIPTOR)
							}
							autoFocus
							data-flx="user.chat-settings-tab.add-custom-search-engine-modal.input.set-name"
						/>
						<Input
							label={i18n._(URL_PATTERN_DESCRIPTOR)}
							value={urlTemplate}
							onChange={(event) => {
								setUrlTemplate(event.target.value);
								setError('');
							}}
							placeholder={exampleUrl}
							error={error}
							data-flx="user.chat-settings-tab.add-custom-search-engine-modal.input.set-url-template"
						/>
						<Modal.Description data-flx="user.chat-settings-tab.add-custom-search-engine-modal.url-hint">
							{hint}
						</Modal.Description>
					</Modal.ContentLayout>
				</Modal.Content>
				<Modal.Footer data-flx="user.chat-settings-tab.add-custom-search-engine-modal.modal-footer">
					<Button
						variant="secondary"
						onClick={handleClose}
						data-flx="user.chat-settings-tab.add-custom-search-engine-modal.button.close"
					>
						{i18n._(CANCEL_DESCRIPTOR)}
					</Button>
					<Button
						variant="primary"
						onClick={handleSubmit}
						data-flx="user.chat-settings-tab.add-custom-search-engine-modal.button.submit"
					>
						{isEditing ? i18n._(SAVE_DESCRIPTOR) : i18n._(ADD_DESCRIPTOR)}
					</Button>
				</Modal.Footer>
			</Modal.Root>
		);
	},
);
