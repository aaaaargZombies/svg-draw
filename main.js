import "./style.css";
import { Elm } from "./src/Main.elm";

if (process.env.NODE_ENV === "development") {
  const ElmDebugTransform = await import("elm-debug-transformer");

  ElmDebugTransform.register({
    simple_mode: true,
  });
}

const root = document.querySelector("#app div");
const app = Elm.Main.init({ node: root });

app.ports.logPort.subscribe((event) => {
  console.log(event);
});

app.ports.printPort.subscribe(async (_msg) => {
  // annoying hacky thing because Vite loads the styles differently to get HMR
  let styles = "";
  if (process.env.NODE_ENV === "development") {
    styles = document.querySelector("style").innerHTML;
  } else {
    const href = document
      .querySelector('link[rel="stylesheet"]')
      .getAttribute("href");

    styles = await fetch(href)
      .then((res) => res.text())
      .catch((_error) => "");
  }

  const meta = `<svg xmlns="http://www.w3.org/2000/svg" `;

  const svgStyles = `><style>
			${styles}
		</style><`;
  const svg =
    '<?xml version="1.0" standalone="no"?>\n' +
    document
      .querySelector("#SVG")
      .outerHTML.replace("<svg", meta)
      .replace("><", svgStyles);

  // prep to save to file
  console.log(svg);
});
