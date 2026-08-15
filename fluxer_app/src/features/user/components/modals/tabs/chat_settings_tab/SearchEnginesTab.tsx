// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {SettingsTabSection} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import Translation from '@app/features/messaging/state/Translation';
import ReverseImageSearch from '@app/features/search/state/ReverseImageSearch';
import SearchEngine from '@app/features/search/state/SearchEngine';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Combobox, type ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {SwitchGroup, SwitchGroupItem} from '@app/features/ui/components/SwitchGroup';
import {
	AddCustomSearchEngineModal,
	type SearchEngineMode,
} from '@app/features/user/components/modals/tabs/chat_settings_tab/AddCustomSearchEngineModal';
import styles from '@app/features/user/components/modals/tabs/chat_settings_tab/SearchEnginesTab.module.css';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {TrashIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const REMOVE_DESCRIPTOR = msg({
	message: 'Remove',
	comment: 'Button or menu action label in the search engines tab. Keep it concise. Keep the tone plain and specific.',
});
const EDIT_DESCRIPTOR = msg({
	message: 'Edit {engineName}',
	comment:
		'Button or menu action label in the search engines tab. Keep it concise. Preserve {engineName}; it is inserted by code.',
});
const EDIT_2_DESCRIPTOR = msg({
	message: 'Edit',
	comment: 'Button or menu action label in the search engines tab. Keep it concise.',
});
const REMOVE_2_DESCRIPTOR = msg({
	message: 'Remove {engineName}',
	comment:
		'Button or menu action label in the search engines tab. Keep it concise. Preserve {engineName}; it is inserted by code. Keep the tone plain and specific.',
});
const DEFAULT_SEARCH_ENGINE_DESCRIPTOR = msg({
	message: 'Default search engine',
	comment: 'Short label in the search engines tab. Keep it concise.',
});
const CHOOSE_WHICH_SEARCH_ENGINE_IS_USED_BY_DEFAULT_DESCRIPTOR = msg({
	message: 'Choose which search engine is used by default when searching selected text.',
	comment: 'Button or menu action label in the search engines tab. Keep it concise.',
});
const BUILT_IN_SEARCH_ENGINES_DESCRIPTOR = msg({
	message: 'Built-in search engines',
	comment: 'Short label in the search engines tab. Keep it concise.',
});
const ENABLE_OR_DISABLE_BUILT_IN_SEARCH_ENGINES_ENABLED_DESCRIPTOR = msg({
	message:
		'Enable or disable built-in search engines. Enabled engines appear in the message context menu when text is selected.',
	comment: 'Button or menu action label in the search engines tab. Keep it concise.',
});
const CUSTOM_SEARCH_ENGINES_DESCRIPTOR = msg({
	message: 'Custom search engines',
	comment: 'Short label in the search engines tab. Keep it concise.',
});
const ADD_SEARCH_ENGINE_DESCRIPTOR = msg({
	message: 'Add search engine',
	comment: 'Button or menu action label in the search engines tab. Keep it concise.',
});
const ENABLE_AT_LEAST_ONE_SEARCH_ENGINE_BELOW_DESCRIPTOR = msg({
	message: 'Enable at least one search engine below.',
	comment: 'Button or menu action label in the search engines tab. Keep it concise.',
});
const REMOVE_SEARCH_ENGINE_DESCRIPTOR = msg({
	message: 'Remove search engine',
	comment: 'Button or menu action label in the search engines tab. Keep it concise. Keep the tone plain and specific.',
});
const DEFAULT_TRANSLATOR_DESCRIPTOR = msg({
	message: 'Default translator',
	comment: 'Short label in the search engines tab. Keep it concise.',
});
const CHOOSE_WHICH_TRANSLATOR_IS_USED_BY_DEFAULT_WHEN_DESCRIPTOR = msg({
	message: 'Choose which translator is used by default when translating selected text.',
	comment: 'Button or menu action label in the search engines tab. Keep it concise.',
});
const BUILT_IN_TRANSLATORS_DESCRIPTOR = msg({
	message: 'Built-in translators',
	comment: 'Short label in the search engines tab. Keep it concise.',
});
const ENABLE_OR_DISABLE_BUILT_IN_TRANSLATORS_ENABLED_TRANSLATORS_DESCRIPTOR = msg({
	message:
		'Enable or disable built-in translators. Enabled translators appear in the message context menu when text is selected.',
	comment: 'Button or menu action label in the search engines tab. Keep it concise.',
});
const CUSTOM_TRANSLATORS_DESCRIPTOR = msg({
	message: 'Custom translators',
	comment: 'Short label in the search engines tab. Keep it concise.',
});
const ADD_TRANSLATOR_DESCRIPTOR = msg({
	message: 'Add translator',
	comment: 'Button or menu action label in the search engines tab. Keep it concise.',
});
const ENABLE_AT_LEAST_ONE_TRANSLATOR_BELOW_DESCRIPTOR = msg({
	message: 'Enable at least one translator below.',
	comment: 'Button or menu action label in the search engines tab. Keep it concise.',
});
const REMOVE_TRANSLATOR_DESCRIPTOR = msg({
	message: 'Remove translator',
	comment: 'Button or menu action label in the search engines tab. Keep it concise. Keep the tone plain and specific.',
});
const DEFAULT_REVERSE_IMAGE_SEARCH_DESCRIPTOR = msg({
	message: 'Default reverse image search',
	comment: 'Label in the search engines tab.',
});
const CHOOSE_WHICH_REVERSE_IMAGE_SEARCH_SERVICE_IS_USED_DESCRIPTOR = msg({
	message: 'Choose which reverse image search service is used by default when searching an image.',
	comment: 'Button or menu action label in the search engines tab. Keep it concise.',
});
const BUILT_IN_REVERSE_IMAGE_SEARCH_DESCRIPTOR = msg({
	message: 'Built-in reverse image search',
	comment: 'Label in the search engines tab.',
});
const ENABLE_OR_DISABLE_BUILT_IN_REVERSE_IMAGE_SEARCH_DESCRIPTOR = msg({
	message:
		'Enable or disable built-in reverse image search providers. Enabled providers appear in the context menu of images, avatars, banners, stickers, and emoji.',
	comment: 'Button or menu action label in the search engines tab. Keep it concise.',
});
const CUSTOM_REVERSE_IMAGE_SEARCH_DESCRIPTOR = msg({
	message: 'Custom reverse image search',
	comment: 'Label in the search engines tab.',
});
const ADD_REVERSE_IMAGE_SEARCH_DESCRIPTOR = msg({
	message: 'Add reverse image search',
	comment: 'Button or menu action label in the search engines tab. Keep it concise.',
});
const ENABLE_AT_LEAST_ONE_REVERSE_IMAGE_SEARCH_PROVIDER_DESCRIPTOR = msg({
	message: 'Enable at least one reverse image search provider below.',
	comment: 'Button or menu action label in the search engines tab. Keep it concise.',
});
const REMOVE_REVERSE_IMAGE_SEARCH_DESCRIPTOR = msg({
	message: 'Remove reverse image search',
	comment: 'Button or menu action label in the search engines tab. Keep it concise. Keep the tone plain and specific.',
});

interface EngineLike {
	id: string;
	name: string;
	urlTemplate: string;
	enabled: boolean;
	isBuiltIn: boolean;
}

interface EngineSource {
	engines: Array<EngineLike>;
	enabledEngines: ReadonlyArray<EngineLike>;
	defaultEngine: EngineLike | null;
	setEnabled: (engineId: string, enabled: boolean) => void;
	setDefaultEngine: (engineId: string) => void;
	removeCustomEngine: (engineId: string) => void;
}

interface EngineSectionProps {
	mode: SearchEngineMode;
	store: EngineSource;
	defaultTitle: string;
	defaultDescription: string;
	builtInTitle: string;
	builtInDescription: string;
	customTitle: string;
	customDescription: React.ReactNode;
	addButtonLabel: string;
	noEnginesText: string;
	confirmTitle: string;
}

const EngineSection: React.FC<EngineSectionProps> = observer(
	({
		mode,
		store,
		defaultTitle,
		defaultDescription,
		builtInTitle,
		builtInDescription,
		customTitle,
		customDescription,
		addButtonLabel,
		noEnginesText,
		confirmTitle,
	}) => {
		const {i18n} = useLingui();
		const enabledEngines = store.enabledEngines;
		const builtInEngines = store.engines.filter((engine) => engine.isBuiltIn);
		const customEngines = store.engines.filter((engine) => !engine.isBuiltIn);
		const defaultEngineOptions: ReadonlyArray<ComboboxOption> = enabledEngines.map((engine) => ({
			value: engine.id,
			label: engine.name,
		}));
		const handleAddCustomEngine = useCallback(() => {
			ModalCommands.push(
				modal(() => (
					<AddCustomSearchEngineModal
						mode={mode}
						data-flx="user.chat-settings-tab.search-engines-tab.handle-add-custom-engine.add-custom-search-engine-modal"
					/>
				)),
			);
		}, [mode]);
		const handleEditCustomEngine = useCallback(
			(engineId: string, currentName: string, currentUrlTemplate: string) => {
				ModalCommands.push(
					modal(() => (
						<AddCustomSearchEngineModal
							mode={mode}
							editingEngineId={engineId}
							initialName={currentName}
							initialUrlTemplate={currentUrlTemplate}
							data-flx="user.chat-settings-tab.search-engines-tab.handle-edit-custom-engine.add-custom-search-engine-modal"
						/>
					)),
				);
			},
			[mode],
		);
		const handleRemoveCustomEngine = useCallback(
			(engineId: string, engineName: string) => {
				ModalCommands.push(
					modal(() => (
						<ConfirmModal
							title={confirmTitle}
							description={
								<Trans>
									Are you sure you want to remove{' '}
									<strong data-flx="user.chat-settings-tab.search-engines-tab.handle-remove-custom-engine.strong">
										{engineName}
									</strong>
									?
								</Trans>
							}
							primaryText={i18n._(REMOVE_DESCRIPTOR)}
							primaryVariant="danger"
							onPrimary={() => {
								store.removeCustomEngine(engineId);
							}}
							data-flx="user.chat-settings-tab.search-engines-tab.handle-remove-custom-engine.confirm-modal"
						/>
					)),
				);
			},
			[confirmTitle, store, i18n],
		);
		return (
			<>
				<SettingsTabSection
					title={defaultTitle}
					description={defaultDescription}
					data-flx="user.chat-settings-tab.search-engines-tab.engine-section.settings-tab-section"
				>
					{enabledEngines.length > 0 ? (
						<Combobox
							value={store.defaultEngine?.id ?? ''}
							options={defaultEngineOptions}
							onChange={(value) => store.setDefaultEngine(value)}
							data-flx="user.chat-settings-tab.search-engines-tab.engine-section.select.set-default-engine"
						/>
					) : (
						<p
							className={styles.noEnginesText}
							data-flx="user.chat-settings-tab.search-engines-tab.engine-section.no-engines-text"
						>
							{noEnginesText}
						</p>
					)}
				</SettingsTabSection>
				<SettingsTabSection
					title={builtInTitle}
					description={builtInDescription}
					data-flx="user.chat-settings-tab.search-engines-tab.engine-section.settings-tab-section--2"
				>
					<SwitchGroup data-flx="user.chat-settings-tab.search-engines-tab.engine-section.switch-group">
						{builtInEngines.map((engine) => (
							<SwitchGroupItem
								key={engine.id}
								label={engine.name}
								value={engine.enabled}
								onChange={(value) => store.setEnabled(engine.id, value)}
								data-flx="user.chat-settings-tab.search-engines-tab.engine-section.switch-group-item.set-enabled"
							/>
						))}
					</SwitchGroup>
				</SettingsTabSection>
				<SettingsTabSection
					title={customTitle}
					description={customDescription}
					data-flx="user.chat-settings-tab.search-engines-tab.engine-section.settings-tab-section--3"
				>
					{customEngines.length > 0 && (
						<SwitchGroup data-flx="user.chat-settings-tab.search-engines-tab.engine-section.switch-group--2">
							{customEngines.map((engine) => (
								<div
									key={engine.id}
									className={styles.customEngineRow}
									data-flx="user.chat-settings-tab.search-engines-tab.engine-section.custom-engine-row"
								>
									<SwitchGroupItem
										label={engine.name}
										value={engine.enabled}
										onChange={(value) => store.setEnabled(engine.id, value)}
										data-flx="user.chat-settings-tab.search-engines-tab.engine-section.switch-group-item.set-enabled--2"
									/>
									<div
										className={styles.customEngineActions}
										data-flx="user.chat-settings-tab.search-engines-tab.engine-section.custom-engine-actions"
									>
										<button
											type="button"
											className={styles.editButton}
											onClick={() => handleEditCustomEngine(engine.id, engine.name, engine.urlTemplate)}
											aria-label={i18n._(EDIT_DESCRIPTOR, {engineName: engine.name})}
											data-flx="user.chat-settings-tab.search-engines-tab.engine-section.edit-button.edit-custom-engine"
										>
											{i18n._(EDIT_2_DESCRIPTOR)}
										</button>
										<button
											type="button"
											className={styles.removeButton}
											onClick={() => handleRemoveCustomEngine(engine.id, engine.name)}
											aria-label={i18n._(REMOVE_2_DESCRIPTOR, {engineName: engine.name})}
											data-flx="user.chat-settings-tab.search-engines-tab.engine-section.remove-button.remove-custom-engine"
										>
											<TrashIcon
												size={remFromPx(16)}
												weight="fill"
												data-flx="user.chat-settings-tab.search-engines-tab.engine-section.trash-icon"
											/>
										</button>
									</div>
								</div>
							))}
						</SwitchGroup>
					)}
					<div
						className={styles.newEngineButtonContainer}
						data-flx="user.chat-settings-tab.search-engines-tab.engine-section.add-button-container"
					>
						<Button
							variant="secondary"
							small
							onClick={handleAddCustomEngine}
							data-flx="user.chat-settings-tab.search-engines-tab.engine-section.button.add-custom-engine"
						>
							{addButtonLabel}
						</Button>
					</div>
				</SettingsTabSection>
			</>
		);
	},
);
export const TextSearchEnginesContent: React.FC = observer(() => {
	const {i18n} = useLingui();
	const query = '{query}';
	return (
		<EngineSection
			mode="text"
			store={SearchEngine}
			defaultTitle={i18n._(DEFAULT_SEARCH_ENGINE_DESCRIPTOR)}
			defaultDescription={i18n._(CHOOSE_WHICH_SEARCH_ENGINE_IS_USED_BY_DEFAULT_DESCRIPTOR)}
			builtInTitle={i18n._(BUILT_IN_SEARCH_ENGINES_DESCRIPTOR)}
			builtInDescription={i18n._(ENABLE_OR_DISABLE_BUILT_IN_SEARCH_ENGINES_ENABLED_DESCRIPTOR)}
			customTitle={i18n._(CUSTOM_SEARCH_ENGINES_DESCRIPTOR)}
			customDescription={
				<Trans>
					Add your own search engines with a custom URL pattern. Use{' '}
					<code data-flx="user.chat-settings-tab.search-engines-tab.text-search-engines-content.code">{query}</code> as
					a placeholder for the search text.
				</Trans>
			}
			addButtonLabel={i18n._(ADD_SEARCH_ENGINE_DESCRIPTOR)}
			noEnginesText={i18n._(ENABLE_AT_LEAST_ONE_SEARCH_ENGINE_BELOW_DESCRIPTOR)}
			confirmTitle={i18n._(REMOVE_SEARCH_ENGINE_DESCRIPTOR)}
			data-flx="user.chat-settings-tab.search-engines-tab.text-search-engines-content.engine-section"
		/>
	);
});

export const TranslatorsContent: React.FC = observer(() => {
	const {i18n} = useLingui();
	const query = '{query}';
	return (
		<EngineSection
			mode="translate"
			store={Translation}
			defaultTitle={i18n._(DEFAULT_TRANSLATOR_DESCRIPTOR)}
			defaultDescription={i18n._(CHOOSE_WHICH_TRANSLATOR_IS_USED_BY_DEFAULT_WHEN_DESCRIPTOR)}
			builtInTitle={i18n._(BUILT_IN_TRANSLATORS_DESCRIPTOR)}
			builtInDescription={i18n._(ENABLE_OR_DISABLE_BUILT_IN_TRANSLATORS_ENABLED_TRANSLATORS_DESCRIPTOR)}
			customTitle={i18n._(CUSTOM_TRANSLATORS_DESCRIPTOR)}
			customDescription={
				<Trans>
					Add your own translators with a custom URL pattern. Use{' '}
					<code data-flx="user.chat-settings-tab.search-engines-tab.translators-content.code">{query}</code> as a
					placeholder for the text to translate.
				</Trans>
			}
			addButtonLabel={i18n._(ADD_TRANSLATOR_DESCRIPTOR)}
			noEnginesText={i18n._(ENABLE_AT_LEAST_ONE_TRANSLATOR_BELOW_DESCRIPTOR)}
			confirmTitle={i18n._(REMOVE_TRANSLATOR_DESCRIPTOR)}
			data-flx="user.chat-settings-tab.search-engines-tab.translators-content.engine-section"
		/>
	);
});

export const ReverseImageSearchContent: React.FC = observer(() => {
	const {i18n} = useLingui();
	const url = '{url}';
	return (
		<EngineSection
			mode="image"
			store={ReverseImageSearch}
			defaultTitle={i18n._(DEFAULT_REVERSE_IMAGE_SEARCH_DESCRIPTOR)}
			defaultDescription={i18n._(CHOOSE_WHICH_REVERSE_IMAGE_SEARCH_SERVICE_IS_USED_DESCRIPTOR)}
			builtInTitle={i18n._(BUILT_IN_REVERSE_IMAGE_SEARCH_DESCRIPTOR)}
			builtInDescription={i18n._(ENABLE_OR_DISABLE_BUILT_IN_REVERSE_IMAGE_SEARCH_DESCRIPTOR)}
			customTitle={i18n._(CUSTOM_REVERSE_IMAGE_SEARCH_DESCRIPTOR)}
			customDescription={
				<Trans>
					Add your own reverse image search providers with a custom URL pattern. Use{' '}
					<code data-flx="user.chat-settings-tab.search-engines-tab.reverse-image-search-content.code">{url}</code> as a
					placeholder for the image URL.
				</Trans>
			}
			addButtonLabel={i18n._(ADD_REVERSE_IMAGE_SEARCH_DESCRIPTOR)}
			noEnginesText={i18n._(ENABLE_AT_LEAST_ONE_REVERSE_IMAGE_SEARCH_PROVIDER_DESCRIPTOR)}
			confirmTitle={i18n._(REMOVE_REVERSE_IMAGE_SEARCH_DESCRIPTOR)}
			data-flx="user.chat-settings-tab.search-engines-tab.reverse-image-search-content.engine-section"
		/>
	);
});

export const SearchEnginesTabContent: React.FC = observer(() => (
	<>
		<TextSearchEnginesContent data-flx="user.chat-settings-tab.search-engines-tab.search-engines-tab-content.text" />
		<TranslatorsContent data-flx="user.chat-settings-tab.search-engines-tab.search-engines-tab-content.translators" />
		<ReverseImageSearchContent data-flx="user.chat-settings-tab.search-engines-tab.search-engines-tab-content.image" />
	</>
));
