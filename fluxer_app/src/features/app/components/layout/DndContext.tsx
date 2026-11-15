// SPDX-License-Identifier: AGPL-3.0-or-later

import {setKeyboardDragActive} from '@app/features/app/components/layout/dnd/KeyboardDragMode';
import type React from 'react';
import {DndProvider} from 'react-dnd';
import KeyboardBackend, {isKeyboardDragTrigger} from 'react-dnd-accessible-backend';
import {HTML5Backend} from 'react-dnd-html5-backend';
import {createTransition, MouseTransition, MultiBackend} from 'react-dnd-multi-backend';

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
			options: {onDndModeChanged: setKeyboardDragActive},
			preview: true,
			transition: KeyboardTransition,
		},
	],
};

interface DndContextProps {
	children: React.ReactNode;
}

export const DndContext = ({children}: DndContextProps) => {
	return (
		<DndProvider backend={MultiBackend} options={DND_OPTIONS} data-flx="app.dnd-context.dnd-provider">
			{children}
		</DndProvider>
	);
};
