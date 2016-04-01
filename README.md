crop-detector
=============

Utility that finds cropped image coordinates inside original image.

<b>Work in progress...</b>

Usage
-----

```bash
crop-detector DIFF_THRESHOLD ORIGINAL_IMAGE CROPPED_IMAGE
```

Where:
  - DIFF_THRESHOLD - Max percent of allowed difference (for JPEG artifacts);
  - ORIGINAL_IMAGE - Path to whole image;
  - CROPPED_IMAGE  - Path to image that is cropped part of ORIGINAL_IMAGE.

Example cmd:
  ```bash
  crop-detector 20 ~/Pictures/original.jpg ~/Pictures/cropped.jpg
  ```

Example output: ```137 260 201 181```
It's: ```x y width height``` of cropped image.

Author
------

Viacheslav Lotsmanov

License
-------

[GPLv3](./LICENSE)
