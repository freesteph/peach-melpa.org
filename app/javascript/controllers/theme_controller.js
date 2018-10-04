import { Controller } from "stimulus";

export default class extends Controller {
  static targets = ["modes", "screenshots", "variants"];

  handleVariantClick({ target }) {
    const nodes = this.variantsTarget.querySelectorAll("li button");
    const index = Array.from(nodes).findIndex(n => n == target);

    const gallery = document.querySelector(".gallery");
    gallery.style = `top: ${
      gallery.style.top
    }; left: calc(100vw * 0.75 * ${-index});`;
  }

  findModeIndex(target) {
    const nodes = Array.from(this.modesTarget.querySelectorAll("li button"));

    return nodes.indexOf(target);
  }

  handleLangClick({ target }) {
    const nodes = this.modesTarget.querySelectorAll("li button");
    const index = Array.from(nodes).findIndex(n => n == target);

    const gallery = document.querySelector(".gallery");
    gallery.style = `top: calc(100vw * 0.75 * 0.75 * ${-index});
       left: ${gallery.style.left};`;
  }
}
