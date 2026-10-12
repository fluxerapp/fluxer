// SPDX-License-Identifier: AGPL-3.0-or-later

import {TRANSLATE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Translation from '@app/features/messaging/state/Translation';
import {SearchProviderPickerModal} from '@app/features/search/components/modals/SearchProviderPickerModal';
import {TranslateIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {SearchProviderContextMenuItems} from '@app/features/ui/action_menu/items/SearchProviderMenuItems';
import {getSearchProviderMenuState} from '@app/features/ui/action_menu/items/SearchProviderMenuUtils';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

interface TranslateMenuItemsProps {
	selectionText: string;
	onClose: () => void;
}

export const TranslateMenuItems: React.FC<TranslateMenuItemsProps> = observer(({selectionText, onClose}) => {
	const {i18n} = useLingui();
	const state = getSearchProviderMenuState(Translation);
	const openWith = useCallback((engineId: string, query: string) => {
		const url = Translation.buildSearchUrl(engineId, query);
		if (url) {
			void openExternalUrl(url);
		}
	}, []);
	const handleTranslateWithEngine = useCallback(
		(engine: {id: string}) => {
			if (!selectionText) return;
			openWith(engine.id, selectionText);
			onClose();
		},
		[selectionText, onClose, openWith],
	);
	const handleDefaultTranslate = useCallback(() => {
		if (!selectionText) return;
		const defaultEngine = Translation.defaultEngine;
		if (defaultEngine) {
			openWith(defaultEngine.id, selectionText);
			onClose();
			return;
		}
		const queryToTranslate = selectionText;
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			modal(() => (
				<SearchProviderPickerModal
					mode="translate"
					onPick={(engineId) => openWith(engineId, queryToTranslate)}
					data-flx="ui.action-menu.items.translate-menu-items.handle-default-translate.search-provider-picker-modal"
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
			defaultLabel={i18n._(TRANSLATE_DESCRIPTOR)}
			renderIcon={() => <TranslateIcon size={20} data-flx="ui.action-menu.items.translate-menu-items.translate-icon" />}
			onDefaultSearch={handleDefaultTranslate}
			onSearchWithEngine={handleTranslateWithEngine}
			data-flx="ui.action-menu.items.translate-menu-items.search-provider-context-menu-items"
		/>
	);
	return <MenuGroup data-flx="ui.action-menu.items.translate-menu-items.menu-group">{content}</MenuGroup>;
});
