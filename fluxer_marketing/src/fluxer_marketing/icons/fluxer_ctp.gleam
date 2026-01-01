//// Copyright (C) 2026 Fluxer Contributors
////
//// This file is part of Fluxer.
////
//// Fluxer is free software: you can redistribute it and/or modify
//// it under the terms of the GNU Affero General Public License as published by
//// the Free Software Foundation, either version 3 of the License, or
//// (at your option) any later version.
////
//// Fluxer is distributed in the hope that it will be useful,
//// but WITHOUT ANY WARRANTY; without even the implied warranty of
//// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//// GNU Affero General Public License for more details.
////
//// You should have received a copy of the GNU Affero General Public License
//// along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

import lustre/attribute
import lustre/element.{type Element}
import lustre/element/svg

pub fn fluxer_ctp(attrs: List(attribute.Attribute(a))) -> Element(a) {
  svg.svg(
    [
      attribute.attribute("xmlns", "http://www.w3.org/2000/svg"),
      attribute.attribute("width", "512"),
      attribute.attribute("height", "512"),
      attribute.attribute("viewBox", "0 0 512 512"),
      attribute.attribute("fill", "none"),
      ..attrs
    ],
    [
      svg.g([attribute.attribute("clip-path", "url(#clip0_80_10)")], [
        svg.path([
          attribute.attribute(
            "d",
            "M96 80H416C424.487 80 432.626 83.3714 438.627 89.3726C444.629 95.3737 448 103.513 448 112V224C448 329.44 396.96 393.34 354.14 428.38C308.02 466.1 262.14 478.92 260.14 479.44C257.39 480.188 254.49 480.188 251.74 479.44C249.74 478.92 203.92 466.1 157.74 428.38C115.04 393.34 64 329.44 64 224V112C64 103.513 67.3714 95.3737 73.3726 89.3726C79.3738 83.3714 87.5131 80 96 80ZM256 447.24C284.296 437.359 310.641 422.596 333.84 403.62C380.34 365.58 407.26 316 414.2 256H256V112H96V224C95.9939 234.692 96.5949 245.376 97.8 256H256V447.24Z",
          ),
          attribute.attribute("fill", "#4641D9"),
        ]),
      ]),
      svg.defs([], [
        svg.clip_path([attribute.attribute("id", "clip0_80_10")], [
          svg.rect([
            attribute.attribute("width", "512"),
            attribute.attribute("height", "512"),
            attribute.attribute("fill", "white"),
          ]),
        ]),
      ]),
    ],
  )
}
