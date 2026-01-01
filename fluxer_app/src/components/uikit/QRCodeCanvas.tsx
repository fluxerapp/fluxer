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

import {observer} from 'mobx-react-lite';
import qrCode from 'qrcode';
import React from 'react';

export const QRCodeCanvas = observer(({data}: {data: string}) => {
	const canvasRef = React.useRef<HTMLCanvasElement>(null);

	React.useEffect(() => {
		const canvas = canvasRef.current;
		const qrSize = 100;
		const padding = 10;
		const totalSize = qrSize + padding * 2;

		if (canvas) {
			canvas.width = totalSize;
			canvas.height = totalSize;
			const context = canvas.getContext('2d');
			if (context) {
				context.fillStyle = 'white';
				context.fillRect(0, 0, totalSize, totalSize);
				context.fillStyle = 'white';
				context.beginPath();
				context.moveTo(padding, 0);
				context.lineTo(totalSize - padding, 0);
				context.quadraticCurveTo(totalSize, 0, totalSize, padding);
				context.lineTo(totalSize, totalSize - padding);
				context.quadraticCurveTo(totalSize, totalSize, totalSize - padding, totalSize);
				context.lineTo(padding, totalSize);
				context.quadraticCurveTo(0, totalSize, 0, totalSize - padding);
				context.lineTo(0, padding);
				context.quadraticCurveTo(0, 0, padding, 0);
				context.closePath();
				context.fill();
				const tempCanvas = document.createElement('canvas');
				qrCode.toCanvas(
					tempCanvas,
					data,
					{width: qrSize, margin: 0, color: {dark: '#000000', light: '#FFFFFF00'}},
					(error: Error | null | undefined) => {
						if (error) {
							console.error(error);
						} else {
							context.drawImage(tempCanvas, padding, padding);
						}
					},
				);
			}
		}
	}, [data]);

	return <canvas ref={canvasRef} style={{borderRadius: 10, backgroundColor: 'white'}} />;
});
