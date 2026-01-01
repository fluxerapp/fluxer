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

import type React from 'react';
import KeyboardBackend, {isKeyboardDragTrigger} from 'react-dnd-accessible-backend';
import {HTML5Backend} from 'react-dnd-html5-backend';
import {createTransition, DndProvider, MouseTransition} from 'react-dnd-multi-backend';

const KeyboardTransition = createTransition('keydown', (event: Event) => {
	if (!isKeyboardDragTrigger(event as KeyboardEvent)) return false;
	event.preventDefault();
	return true;
});

const DND_OPTIONS = {
	backends: [
		{
			id: 'html5',
			backend: HTML5Backend,
			transition: MouseTransition,
		},
		{
			id: 'keyboard',
			backend: KeyboardBackend,
			context: {window, document},
			preview: true,
			transition: KeyboardTransition,
		},
	],
};

interface DndContextProps {
	children: React.ReactNode;
}

export const DndContext = ({children}: DndContextProps) => {
	return <DndProvider options={DND_OPTIONS}>{children}</DndProvider>;
};
