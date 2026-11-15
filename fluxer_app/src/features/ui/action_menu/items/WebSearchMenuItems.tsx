// SPDX-License-Identifier: AGPL-3.0-or-later

import {SearchProviderPickerModal} from '@app/features/search/components/modals/SearchProviderPickerModal';
import SearchEngine from '@app/features/search/state/SearchEngine';
import {SearchIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {SearchProviderContextMenuItems} from '@app/features/ui/action_menu/items/SearchProviderMenuItems';
import {getSearchProviderMenuState} from '@app/features/ui/action_menu/items/SearchProviderMenuUtils';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const SEARCH_THE_WEB_DESCRIPTOR = msg({
	message: 'Search the web',
	comment: 'Action that opens a web search for the selected text or media.',
});

interface WebSearchMenuItemsProps {
	selectionText: string;
	onClose: () => void;
}

export const WebSearchMenuItems: React.FC<WebSearchMenuItemsProps> = observer(({selectionText, onClose}) => {
	const {i18n} = useLingui();
	const state = getSearchProviderMenuState(SearchEngine);
	const openWith = useCallback((engineId: string, query: string) => {
		const url = SearchEngine.buildSearchUrl(engineId, query);
		if (url) {
			void openExternalUrl(url);
		}
	}, []);
	const handleSearchWithEngine = useCallback(
		(engine: {id: string}) => {
			if (!selectionText) return;
			openWith(engine.id, selectionText);
			onClose();
		},
		[selectionText, onClose, openWith],
	);
	const handleDefaultSearch = useCallback(() => {
		if (!selectionText) return;
		const defaultEngine = SearchEngine.defaultEngine;
		if (defaultEngine) {
			openWith(defaultEngine.id, selectionText);
			onClose();
			return;
		}
		const queryToSearch = selectionText;
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			modal(() => (
				<SearchProviderPickerModal
					mode="text"
					onPick={(engineId) => openWith(engineId, queryToSearch)}
					data-flx="ui.action-menu.items.web-search-menu-items.handle-default-search.search-provider-picker-modal"
				/>
			)),
		);
	}, [selectionText, onClose, openWith]);
	if (!selectionText || state.enabledEngines.length === 0) {
		return null;
	}
	const content = (
		<SearchProviderContextMenuItems
			state={state}
			defaultLabel={i18n._(SEARCH_THE_WEB_DESCRIPTOR)}
			renderIcon={() => <SearchIcon size={20} data-flx="ui.action-menu.items.web-search-menu-items.search-icon" />}
			onDefaultSearch={handleDefaultSearch}
			onSearchWithEngine={handleSearchWithEngine}
			data-flx="ui.action-menu.items.web-search-menu-items.search-provider-context-menu-items"
		/>
	);
	return <MenuGroup data-flx="ui.action-menu.items.web-search-menu-items.menu-group">{content}</MenuGroup>;
});
