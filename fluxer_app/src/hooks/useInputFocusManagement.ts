/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import React from 'react';
import {canFocusTextarea, type FocusableElementType, isInputFocused, safeFocus} from '~/lib/InputFocusManager';

export const useInputFocusManagement = (textareaRef: React.RefObject<FocusableElementType | null>) => {
	const safeFocusTextarea = React.useCallback(
		(force: boolean = false) => {
			if (!textareaRef.current) return false;
			return safeFocus(textareaRef.current, force);
		},
		[textareaRef],
	);

	const canFocus = React.useCallback(() => {
		if (!textareaRef.current) return false;
		return canFocusTextarea(textareaRef.current);
	}, [textareaRef]);

	const hasOtherInputFocused = React.useCallback(() => {
		return isInputFocused(textareaRef.current || undefined);
	}, [textareaRef]);

	return {
		safeFocusTextarea,
		canFocus,
		hasOtherInputFocused,
		isInputFocused: () => isInputFocused(textareaRef.current || undefined),
	};
};
