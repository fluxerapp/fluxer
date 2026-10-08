// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {CANCEL_DESCRIPTOR, TRANSLATE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Translation from '@app/features/messaging/state/Translation';
import ReverseImageSearch from '@app/features/search/state/ReverseImageSearch';
import SearchEngine from '@app/features/search/state/SearchEngine';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Combobox, type ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {formatUserSettingsPath} from '@app/features/user/components/settings_utils/SettingsConstants';
import {msg, ph} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo, useState} from 'react';

const PICK_A_REVERSE_IMAGE_SEARCH_PROVIDER_DESCRIPTOR = msg({
	message: 'Pick a reverse image search provider',
	comment: 'Title of the picker modal when choosing a reverse image search provider.',
});
const PICK_A_TRANSLATION_PROVIDER_DESCRIPTOR = msg({
	message: 'Pick a translation provider',
	comment: 'Title of the picker modal when choosing a translation provider.',
});
const PICK_A_WEB_SEARCH_PROVIDER_DESCRIPTOR = msg({
	message: 'Pick a web search provider',
	comment: 'Title of the picker modal when choosing a web search provider.',
});
const REVERSE_IMAGE_SEARCH_PROVIDER_DESCRIPTOR = msg({
	message: 'Reverse image search provider',
	comment: 'Accessible label for the radio group in the reverse image search provider picker.',
});
const TRANSLATION_PROVIDER_DESCRIPTOR = msg({
	message: 'Translation provider',
	comment: 'Accessible label for the radio group in the translation provider picker.',
});
const WEB_SEARCH_PROVIDER_DESCRIPTOR = msg({
	message: 'Web search provider',
	comment: 'Accessible label for the radio group in the web search provider picker.',
});
const SEARCH_DESCRIPTOR = msg({
	message: 'Search',
	comment: 'Confirm button label in the reverse image search provider picker. Initiates the search.',
});
const SEARCH_THE_WEB_DESCRIPTOR = msg({
	message: 'Search the web',
	comment: 'Confirm button label in the web search provider picker. Initiates the web search.',
});

type SearchProviderPickerMode = 'text' | 'image' | 'translate';

interface SearchProviderPickerModalProps {
	mode: SearchProviderPickerMode;
	onPick: (engineId: string) => void;
}

export const SearchProviderPickerModal: React.FC<SearchProviderPickerModalProps> = observer(({mode, onPick}) => {
	const {i18n} = useLingui();
	const isImageMode = mode === 'image';
	const isTranslateMode = mode === 'translate';
	const searchEnginesSettingsPath = formatUserSettingsPath(i18n, 'advanced_settings');
	const store = isImageMode ? ReverseImageSearch : isTranslateMode ? Translation : SearchEngine;
	const engines = store.engines;
	const options = useMemo<ReadonlyArray<ComboboxOption>>(
		() => engines.map((engine) => ({value: engine.id, label: engine.name})),
		[engines],
	);
	const [selectedEngineId, setSelectedEngineId] = useState<string>(
		() => store.defaultEngine?.id ?? store.effectiveDefaultEngine?.id ?? engines[0]?.id ?? '',
	);
	const [submitting, setSubmitting] = useState(false);
	const handleClose = useCallback(() => {
		ModalCommands.pop();
	}, []);
	const handleConfirm = useCallback(async () => {
		const engineId = engines.find((engine) => engine.id === selectedEngineId)?.id;
		if (!engineId) return;
		setSubmitting(true);
		try {
			await store.setDefaultEngine(engineId);
		} finally {
			setSubmitting(false);
		}
		ModalCommands.pop();
		onPick(engineId);
	}, [engines, onPick, selectedEngineId, store]);
	const title = isImageMode
		? i18n._(PICK_A_REVERSE_IMAGE_SEARCH_PROVIDER_DESCRIPTOR)
		: isTranslateMode
			? i18n._(PICK_A_TRANSLATION_PROVIDER_DESCRIPTOR)
			: i18n._(PICK_A_WEB_SEARCH_PROVIDER_DESCRIPTOR);
	const description = isImageMode ? (
		<Trans>
			Choose where {ph({PRODUCT_NAME: RuntimeConfig.productName})} should send images for reverse image search. We'll
			remember your pick. You can change it or add your own provider later in {searchEnginesSettingsPath}.
		</Trans>
	) : isTranslateMode ? (
		<Trans>
			Choose where {ph({PRODUCT_NAME: RuntimeConfig.productName})} should send highlighted text for translation. We'll
			remember your pick. You can change it or add your own provider later in {searchEnginesSettingsPath}.
		</Trans>
	) : (
		<Trans>
			Choose where {ph({PRODUCT_NAME: RuntimeConfig.productName})} should search highlighted text. We'll remember your
			pick. You can change it or add your own provider later in {searchEnginesSettingsPath}.
		</Trans>
	);
	const selectLabel = isImageMode
		? i18n._(REVERSE_IMAGE_SEARCH_PROVIDER_DESCRIPTOR)
		: isTranslateMode
			? i18n._(TRANSLATION_PROVIDER_DESCRIPTOR)
			: i18n._(WEB_SEARCH_PROVIDER_DESCRIPTOR);
	const confirmLabel = isImageMode
		? i18n._(SEARCH_DESCRIPTOR)
		: isTranslateMode
			? i18n._(TRANSLATE_DESCRIPTOR)
			: i18n._(SEARCH_THE_WEB_DESCRIPTOR);
	const hasSelectedEngine = engines.some((engine) => engine.id === selectedEngineId);
	return (
		<Modal.Root size="small" onClose={handleClose} data-flx="search.search-provider-picker-modal.modal-root">
			<Modal.Header title={title} onClose={handleClose} data-flx="search.search-provider-picker-modal.modal-header" />
			<Modal.Content data-flx="search.search-provider-picker-modal.modal-content">
				<Modal.ContentLayout data-flx="search.search-provider-picker-modal.modal-content-layout">
					<Modal.Description data-flx="search.search-provider-picker-modal.modal-description">
						{description}
					</Modal.Description>
					<Combobox
						label={selectLabel}
						value={selectedEngineId}
						options={options}
						onChange={setSelectedEngineId}
						data-flx="search.search-provider-picker-modal.select"
					/>
				</Modal.ContentLayout>
			</Modal.Content>
			<Modal.Footer data-flx="search.search-provider-picker-modal.modal-footer">
				<Button variant="secondary" onClick={handleClose} data-flx="search.search-provider-picker-modal.button.close">
					{i18n._(CANCEL_DESCRIPTOR)}
				</Button>
				<Button
					variant="primary"
					onClick={handleConfirm}
					disabled={!hasSelectedEngine || submitting}
					submitting={submitting}
					data-flx="search.search-provider-picker-modal.button.confirm"
				>
					{confirmLabel}
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
